open Ast
open Symbol_table
module L = Llvm


let theContext = L.global_context()
let theModule = L.create_module theContext "main_module"
let int_t  = L.i32_type  theContext
and char_t = L.i8_type theContext
and bool_t = L.i1_type theContext
and void_t = L.void_type theContext

let c_zero = L.const_int int_t 0
let c_one  = L.const_int int_t 1

let rec ltype_of_typ = function
    TypI -> int_t
  | TypB -> bool_t
  | TypC -> char_t
  | TypV -> void_t
  | TypP t -> L.pointer_type (ltype_of_typ t)
  | TypA (t, d) -> match d with
        |None   -> L.array_type (ltype_of_typ t) 0 (*Clang implementation*)
        |Some n -> L.array_type (ltype_of_typ t) n

let declareLibraryFuns = 
  let print_t = L.function_type void_t [|int_t|] in
  let _ = L.declare_function "print" print_t theModule in
  let getint_t = L.function_type int_t [||] in
  L.declare_function "getint" getint_t theModule

let declareFun {typ; fname; formals; body;} = 
  let returnT = ltype_of_typ typ in
  let formalsT = Array.of_list(
    List.map (fun (t, id) -> ltype_of_typ t) formals
    ) in
  let funT = L.function_type returnT formalsT in
  L.define_function fname funT theModule 

let declareVar (t, id) = 
  let init =  L.const_null (ltype_of_typ t) in
  L.define_global id init theModule

let allocLocalVar (t, id) builder = 
  L.build_alloca (ltype_of_typ t) id builder

(*Allocates a t typed variable on the stack, and stores
the actual parameters in it*)
let allocateParams builder env (t, id) v = 
  L.set_value_name id v;
  let ltype = ltype_of_typ t in
  let addr = L.build_alloca ltype (id^"_addr") builder in
  let _ = L.build_store v addr builder in
  add_entry id addr env


  

let rec buildExpr env builder {loc; node;} = 
  match node with
  | ILiteral n           -> L.const_int int_t n     
  | CLiteral c           -> L.const_int char_t (Char.code c) 
  | BLiteral b           -> L.const_int bool_t (Bool.to_int b)      
  | Access a             ->  
    (*here a is used as a R-value, so it must be loaded*)
    let addr = buildAcc env builder a in
       L.build_load addr "" builder
  | Addr a               -> 
    (*here we need the address of a, which is return buy buildAcc*)
    buildAcc env builder a
  | Assign(a, e)         -> 
      let v = buildExpr env builder e in
      let addr = buildAcc env builder a in
        L.build_store v addr builder |> ignore;
      v
  | PostIncr a -> let addr = buildAcc env builder a in
                  let oldV =  L.build_load addr "" builder in
                  let newV = L.build_add oldV c_one "incr" builder in
                  L.build_store newV addr builder |> ignore;
                  oldV
  | PostDecr a -> let addr = buildAcc env builder a in
                  let oldV =  L.build_load addr "" builder in
                  let newV = L.build_sub oldV c_one "incr" builder in
                  L.build_store newV addr builder |> ignore;
                  oldV
  | PreIncr  a -> let addr = buildAcc env builder a in
                  let oldV =  L.build_load addr "" builder in
                  let newV = L.build_add oldV c_one "incr" builder in
                  L.build_store newV addr builder |> ignore;
                  newV
  | PreDecr  a -> let addr = buildAcc env builder a in
                  let oldV =  L.build_load addr "" builder in
                  let newV = L.build_sub oldV c_one "incr" builder in
                  L.build_store newV addr builder |> ignore;
                  newV
  | UnaryOp(op, e)       -> 
      let v = buildExpr env builder e in
      (match (op) with
      |Neg -> L.build_neg v "neg_result" builder
      |Not -> L.build_not v "not_result" builder   
      )      
  | BinaryOp(op, e1, e2) -> 
      buildBinOp env builder op e1 e2
  | Call(id, args)       -> 
      let f = L.lookup_function id theModule |> Option.get in
      let actuals = List.map (buildExpr env builder) args |> Array.of_list in
      L.build_call f actuals "" builder
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
      L.build_gep array [|c_zero; index|] "array_addr" builder
and buildBinOp env builder op e1 e2 = 
  let v1 = buildExpr env builder e1 in 
  let v2 = buildExpr env builder e2 in 
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

let ifNoTerminator buildTerminator builder=
  let block = L.insertion_block builder in
  if Option.is_none (L.block_terminator block) 
    then buildTerminator builder |> ignore
    else ()

let rec buildStmt env builder fundef {loc; node;} =
  match node with
  | Block list      -> buildBlock env builder fundef list
  | Expr e          -> buildExpr env builder e |> ignore           
  | If(e, s1, s2)   -> 
      let guard = buildExpr env builder e in
      let thenBlock = L.append_block theContext "then" fundef in
      let elseBlock = L.append_block theContext "else" fundef in
      let mergeBlock = L.append_block theContext "merge" fundef in
        L.build_cond_br guard thenBlock elseBlock builder |> ignore;
        L.position_at_end thenBlock builder;
          buildStmt env builder fundef s1 |> ignore;
        L.position_at_end elseBlock builder;
          buildStmt env builder fundef s2 |> ignore;
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
    let guardBlock = L.append_block theContext "guard" fundef in
    let loopBlock = L.append_block theContext "loop" fundef in
    let contBlock = L.append_block theContext "continuation" fundef in
      L.build_br guardBlock builder |> ignore;
      L.position_at_end guardBlock builder;
        let guard = buildExpr env builder e in
        L.build_cond_br guard loopBlock contBlock builder |> ignore;
      L.position_at_end loopBlock builder;
        buildStmt env builder fundef s;
        ifNoTerminator 
          (L.build_br guardBlock) builder;
      L.position_at_end contBlock builder
  | Return eOpt -> 
      if Option.is_none eOpt then 
        L.build_ret_void builder |> ignore
      else 
        let v = buildExpr env builder (Option.get eOpt) in
        L.build_ret v builder |> ignore
and buildBlock env builder fundef l = 
  let blockEnv = env |> begin_block in
    List.fold_left (
      fun env n -> match n.node with
      |Dec (t, id)  -> add_entry id (allocLocalVar (t, id) builder) env
      |Stmt s       -> buildStmt env builder fundef s; env
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
      |Block l -> buildBlock actualsEnv builder d l  
      |_ -> failwith "Illegal AST: function body must be a block");
    match typ with
      |TypV -> ifNoTerminator
                (L.build_ret_void) builder
      |_ -> ()
  

let to_ir (Prog(topdecls)) : L.llmodule =
  L.set_target_triple "x86_64-pc-linux-gnu" theModule;
  declareLibraryFuns |> ignore ;
  List.iter ( 
    fun d -> match d.node with
    |Vardec(t, id)  ->  declareVar (t, id) |> ignore
    |Fundecl f      ->  buildFunction f
  ) topdecls;
  theModule