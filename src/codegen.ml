open Ast
open Symbol_table
module L = Llvm


let theContext = L.global_context()
let theModule = L.create_module theContext "main_module"
let int_t  = L.i32_type  theContext
and double_t = L.double_type theContext
and char_t = L.i8_type theContext
and bool_t = L.i1_type theContext
and void_t = L.void_type theContext

let c_zero = L.const_int int_t 0
let c_one  = L.const_int int_t 1

let rec ltype_of_typ = function
    TypI -> int_t
  | TypD -> double_t
  | TypB -> bool_t
  | TypC -> char_t
  | TypV -> void_t
  | TypNullP -> int_t
  | TypP t -> L.pointer_type (ltype_of_typ t)
  | TypA (t, d) -> match d with
    |None   -> L.array_type (ltype_of_typ t) 0 (*Clang implementation*)
    |Some n -> L.array_type (ltype_of_typ t) n

let decayed_ltype_of_typ = function
  |TypA(t, _) -> L.pointer_type (ltype_of_typ t)
  |t -> ltype_of_typ t

let declareLibraryFuns = 
  let print_t = L.function_type void_t [|int_t|] in
  L.declare_function "print" print_t theModule |> ignore;
  let printChar_t = L.function_type void_t [|char_t|] in
  L.declare_function "print_char" printChar_t theModule |> ignore;
  let getint_t = L.function_type int_t [||] in
  L.declare_function "getint" getint_t theModule |> ignore;
  let printDouble_t = L.function_type void_t [|double_t|] in
  L.declare_function "print_double" printDouble_t theModule

let declareFun {typ; fname; formals; body;} = 
  let returnT = ltype_of_typ typ in
  let formalsT = Array.of_list(
    List.map (fun (t, id) ->decayed_ltype_of_typ t) formals
    ) in
  let funT = L.function_type returnT formalsT in
  L.define_function fname funT theModule 

let declareGlobalVar {loc; node=(t, id, v)} = 
  let init =  match v with
    |None -> L.const_null (ltype_of_typ t) 
    |Some {loc; node=n} -> (match n with
      | ILiteral n       -> L.const_int int_t n   
      | DLiteral n       -> L.const_float double_t n  
      | CLiteral c       -> L.const_int char_t (Char.code c) 
      | BLiteral b       -> L.const_int bool_t (Bool.to_int b)   
      | SLiteral s       -> L.const_stringz theContext s
      | Addr {loc; node=AccVar i} ->
          L.lookup_global i theModule |> Option.get
      |_                 -> failwith "non constant global value inizializer" 
    ) 
  in
  L.define_global id init theModule


(*Allocates a t typed variable on the stack, and stores
the actual parameters in it*)
let allocateParams builder env (t, id) v = 
  L.set_value_name id v;
  let ltype = decayed_ltype_of_typ t in
  let addr = L.build_alloca ltype (id^"_addr") builder in
  let _ = L.build_store v addr builder in
  add_entry id addr env


let isArray v = 
  match L.classify_type (L.element_type(L.type_of v)) with
  |L.TypeKind.Array -> true
  |_ -> false

let castIfNull value t builder = 
  match L.classify_value value with
    |L.ValueKind.ConstantPointerNull ->
      L.build_inttoptr value t "cast" builder
    |_ -> value


let castIfCoercion value t builder =
  let tFrom = L.classify_type (L.type_of value) in
  let tTo = L.classify_type t in
  match (tFrom, tTo) with
  |(L.TypeKind.Integer, L.TypeKind.Double) ->
    L.build_sitofp value double_t "conv" builder
  |(L.TypeKind.Double, L.TypeKind.Integer) ->
    L.build_fptosi value int_t "conv" builder
  |_ -> value

let rec buildExpr env builder {loc; node;} = 
  match node with
  | ILiteral n           -> L.const_int int_t n     
  | DLiteral n           -> L.const_float double_t n
  | CLiteral c           -> L.const_int char_t (Char.code c) 
  | BLiteral b           -> L.const_int bool_t (Bool.to_int b)   
  | SLiteral s           -> let global = L.build_global_string s "const_str" builder 
                            in L.build_load global "temp" builder  
  | NullLiteral          -> L.const_pointer_null int_t
  | Access a             ->  
    (*here a is used as a R-value, so it must be loaded*)
    let addr = buildAcc env builder a in
    L.build_load addr "" builder
  | Addr a               -> 
    (*here we need the address of a, which is returned by buildAcc*)
    buildAcc env builder a
  | Assign(a, e, _op)         -> 
      let addr = buildAcc env builder a in
      let destT = L.element_type(L.type_of addr) in
      let value = buildExpr env builder e in
      let value = match _op with
         None -> value
        |Some(op) -> 
          let oldV =  L.build_load addr "" builder in 
          buildBinOp env builder op oldV value
      in
      let value = castIfNull value destT builder in
      let value = castIfCoercion value destT builder in
      L.build_store value addr builder |> ignore;
      value
  | PostIncr a -> let addr = buildAcc env builder a in
                  let oldV =  L.build_load addr "" builder in
                  let newV = buildBinOp env builder Add oldV c_one in
                  L.build_store newV addr builder |> ignore;
                  oldV
  | PostDecr a -> let addr = buildAcc env builder a in
                  let oldV =  L.build_load addr "" builder in
                  let newV = buildBinOp env builder Sub oldV c_one in
                  L.build_store newV addr builder |> ignore;
                  oldV
  | PreIncr  a -> let addr = buildAcc env builder a in
                  let oldV =  L.build_load addr "" builder in
                  let newV = buildBinOp env builder Add oldV c_one in
                  L.build_store newV addr builder |> ignore;
                  newV
  | PreDecr  a -> let addr = buildAcc env builder a in
                  let oldV =  L.build_load addr "" builder in
                  let newV = buildBinOp env builder Sub oldV c_one in
                  L.build_store newV addr builder |> ignore;
                  newV
  | UnaryOp(op, e)       -> 
      let v = buildExpr env builder e in
      (match (op) with
      |Neg -> L.build_neg v "neg_result" builder
      |Not -> L.build_not v "not_result" builder   
      )      
  | BinaryOp(op, e1, e2) -> 
      let v1 = buildExpr env builder e1 in 
      let v2 = buildExpr env builder e2 in 
      let v2 = castIfNull v2 (L.type_of v1) builder in
      buildBinOp env builder op v1 v2
  | Call(id, args)       -> 
      buildCall env builder id args
      
and buildAcc env builder {loc; node} =
  match node with
    |AccVar id -> 
      (match lookup id env with
      |Some addr -> addr
      |None -> L.lookup_global id theModule |> Option.get)
    |AccDeref e -> 
      buildExpr env builder e 
    |AccIndex(a, e) -> 
      let index = buildExpr env builder e in
      let array = buildAcc env builder a in
      match L.classify_type (L.element_type (L.type_of array)) with
      |L.TypeKind.Array ->
        L.build_gep array [|c_zero; index|] "elem_addr" builder
      |L.TypeKind.Pointer ->
        let first_el = L.build_load array "base_addr" builder in
        L.build_gep first_el [|index|] "elem_addr" builder
      |_ -> failwith "Accessing not an array"
and buildBinOp env builder op v1 v2 =  
  if (L.type_of v1 == double_t) || (L.type_of v2 == double_t)
    then buildDoubleBinOp env builder op v1 v2
  else
    match op with 
    |Add      -> L.build_add v1 v2 "add_result" builder
    |Sub      -> L.build_sub v1 v2 "sub_result" builder
    |Mult     -> L.build_mul v1 v2 "mul_result" builder
    |Div      -> L.build_sdiv v1 v2 "div_result" builder
    |Mod      -> L.build_srem v1 v2 "rem_result" builder
    |Equal    -> L.build_icmp Eq v1 v2 "eq_result" builder 
    |Neq      -> L.build_icmp Ne v1 v2 "neq_result" builder
    |Less     -> L.build_icmp Slt v1 v2 "less_result" builder
    |Leq      -> L.build_icmp Sle v1 v2 "leq_result" builder
    |Greater  -> L.build_icmp Sgt v1 v2 "greater_result" builder
    |Geq      -> L.build_icmp Sge v1 v2 "geq_result" builder
    |And      -> L.build_and v1 v2 "and_result" builder
    |Or       -> L.build_or v1 v2 "or_result" builder
and buildDoubleBinOp env builder op v1 v2 =
  let v1 = castIfCoercion v1 double_t builder in
  let v2 = castIfCoercion v2 double_t builder in
  match op with
  |Add      -> L.build_fadd v1 v2 "add_result" builder
  |Sub      -> L.build_fsub v1 v2 "sub_result" builder
  |Mult     -> L.build_fmul v1 v2 "mul_result" builder
  |Div      -> L.build_fdiv v1 v2 "div_result" builder
  |Mod      -> L.build_frem v1 v2 "rem_result" builder
  |Equal    -> L.build_fcmp Oeq v1 v2 "eq_result" builder 
  |Neq      -> L.build_fcmp One v1 v2 "neq_result" builder
  |Less     -> L.build_fcmp Olt v1 v2 "less_result" builder
  |Leq      -> L.build_fcmp Ole v1 v2 "leq_result" builder
  |Greater  -> L.build_fcmp Ogt v1 v2 "greater_result" builder
  |Geq      -> L.build_fcmp Oge v1 v2 "geq_result" builder
  |_        -> failwith "float operand on not arithmetic operation"
  and buildCall env builder id args = 
    let build_decayed_expr e = match e with
      |{loc; node=SLiteral s} -> 
          let addr = L.build_global_string s "const_str" builder in 
          L.build_gep addr [|c_zero; c_zero|] "array_decay" builder
      |{loc; node=Access a} -> 
        let addr = buildAcc env builder a in
        if isArray addr then 
          L.build_gep addr [|c_zero; c_zero|] "array_decay" builder
        else
          L.build_load addr "" builder
      |e -> buildExpr env builder e in
    let actuals = List.map build_decayed_expr args 
    in
    let f = L.lookup_function id theModule |> Option.get in
    let formals = L.params f |> Array.to_list 
    in let casted_actuals = List.map2 (
      fun actual formal ->
        let destT = L.type_of formal in
        let actual = castIfNull actual destT builder in
        let actual = castIfCoercion actual destT builder in
        actual
    ) actuals formals in
    L.build_call f (Array.of_list casted_actuals) "" builder
    
  

let allocLocalVar env builder {loc; node=(t, id, v)} = 
  match v with
  |None -> 
    L.build_alloca (ltype_of_typ t) id builder
  |Some e ->
    let value = buildExpr env builder e in
    let value = castIfNull value (ltype_of_typ t) builder in
    let value = castIfCoercion value (ltype_of_typ t) builder in
    let value_t = L.type_of value in
    (*Thanks to semantic cheking, we are sure that value_t
    is a compatible value with t*)
    let address = L.build_alloca value_t id builder in
    let _ = L.build_store value address builder in
    address


let ifNoTerminator buildTerminator builder=
  let block = L.insertion_block builder in
  if Option.is_none (L.block_terminator block) 
    then buildTerminator builder |> ignore
    else ()
    


let rec buildStmt env builder {loc; node;} =
  let currentBlock = L.insertion_block builder in
  let currentFun = L.block_parent currentBlock in
  (*Instruction after return are ignored*)
  if Option.is_none (L.block_terminator currentBlock) then
  match node with
  | Block list      -> buildBlock env builder list
  | Expr e          -> buildExpr env builder e |> ignore           
  | If(e, s1, s2)   -> 
      let guard = buildExpr env builder e in
      let thenBlock = L.append_block theContext "then" currentFun in
      let elseBlock = L.append_block theContext "else" currentFun in
      let mergeBlock = L.append_block theContext "merge" currentFun in
        L.build_cond_br guard thenBlock elseBlock builder |> ignore;
        L.position_at_end thenBlock builder;
          buildStmt env builder s1 |> ignore;
        L.position_at_end elseBlock builder;
          buildStmt env builder s2 |> ignore;
        (match (L.block_terminator thenBlock, 
              L.block_terminator elseBlock) 
         with
          |None, None -> 
            L.position_at_end thenBlock builder;
              L.build_br mergeBlock builder |> ignore;
            L.position_at_end elseBlock builder;
              L.build_br mergeBlock builder |> ignore;
            L.position_at_end mergeBlock builder
          |None, Some _ ->
            L.position_at_end thenBlock builder;
              L.build_br mergeBlock builder |> ignore;
            L.position_at_end mergeBlock builder
          |Some _, None ->
            L.position_at_end elseBlock builder;
              L.build_br mergeBlock builder |> ignore;
            L.position_at_end mergeBlock builder
          |Some _, Some _ -> L.delete_block mergeBlock)
  | While(e, s)     -> 
    let guardBlock = L.append_block theContext "guard" currentFun in
    let loopBlock = L.append_block theContext "loop" currentFun in
    let contBlock = L.append_block theContext "continuation" currentFun in
      L.build_br guardBlock builder |> ignore;
      L.position_at_end guardBlock builder;
        let guard = buildExpr env builder e in
        L.build_cond_br guard loopBlock contBlock builder |> ignore;
      L.position_at_end loopBlock builder;
        buildStmt env builder s;
        ifNoTerminator 
          (L.build_br guardBlock) builder;
      L.position_at_end contBlock builder
  | Return eOpt -> 
      if Option.is_none eOpt then 
        L.build_ret_void builder |> ignore
      else 
        let v = buildExpr env builder (Option.get eOpt) in
        L.build_ret v builder |> ignore
and buildBlock env builder l = 
  let blockEnv = env |> begin_block in
    List.fold_left (
      fun env n -> match n.node with
      |Localdec d -> 
          let {loc; node=(t, id, c)} = d in
          let address = allocLocalVar env builder d in
            add_entry id address env
      |Stmt s     -> 
          buildStmt env builder s; env
    ) blockEnv l 
  |> ignore

let buildFunction ({typ; fname; formals; body;} as f) =
    let d = declareFun f in
    let builder = L.builder_at_end theContext (L.entry_block d) in
    let actuals = L.params d |> Array.to_list in
    let actualsEnv = List.fold_left2 
      (allocateParams builder) empty_table formals actuals
    in
    (match body.node with 
      |Block l -> buildBlock actualsEnv builder l  
      |_ -> failwith "Illegal AST: function body must be a block");
    if Option.is_none (L.block_terminator (L.insertion_block builder)) 
      then match typ with 
      |TypV -> L.build_ret_void builder |> ignore
      |_ -> (Util.raise_semantic_error dummy_pos "missing return statement")
  

let to_ir (Prog(topdecls)) : L.llmodule =
  L.set_target_triple "x86_64-pc-linux-gnu" theModule;
  declareLibraryFuns |> ignore ;
  List.iter ( 
    fun d -> match d.node with
    |Globaldec d    ->  declareGlobalVar d |> ignore
    |Fundecl f      ->  buildFunction f
  ) topdecls;
  (*let n = L.const_pointer_null int_t in
  let s = match L.classify_value n with
    |L.ValueKind.ConstantPointerNull -> "const ptr null"
    |L.ValueKind.NullValue -> "null value"
    |L.ValueKind.ConstantInt -> "const int"
    |_ -> "altro" in
  Printf.printf "%s\n" (L.string_of_llvalue n);
  Printf.printf "%s\n" (s);
  let t = L.pointer_type bool_t in
  let s = L.string_of_lltype ( t) in
  Printf.printf "%s\n" s;*)
  theModule