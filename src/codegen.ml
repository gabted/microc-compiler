open Ast
open Symbol_table
module L = Llvm


let theContext = L.global_context()
let theModule = L.create_module theContext "main_module"
let int_t  = L.i32_type  theContext
and char_t = L.i8_type theContext
and bool_t = L.i1_type theContext
and void_t = L.void_type theContext

let rec ltype_of_typ = function
    TypI -> int_t
  | TypB -> bool_t
  | TypC -> char_t
  | TypV -> void_t
  | TypP t -> L.pointer_type (ltype_of_typ t)
  | TypA (t, d) -> match d with
        |None   -> L.array_type (ltype_of_typ t) 1 (*Clang implementation*)
        |Some n -> L.array_type (ltype_of_typ t) n

let declareVar (t, id) = 
  let init =  L.const_null (ltype_of_typ t) in
  L.define_global id init theModule

let declareFun {typ; fname; formals; body;} = 
  let returnT = ltype_of_typ typ in
  let formalsT = Array.of_list(
    List.map (fun (t, id) -> ltype_of_typ t) formals
    ) in
  let funT = L.function_type returnT formalsT in
  L.define_function fname funT theModule 

let declareLocalVar (t, id) builder = 
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
  | Access a             ->  (*a here is used as a R-value*)
    let v = buildAcc env builder a in
       L.build_load v "" builder
  | Addr a               -> buildAcc env builder a
  | Assign(a, e)         -> 
      let v = buildExpr env builder e in
      let addr = buildAcc env builder a in
        L.build_store v addr builder
  | UnaryOp(op, e)       -> 
      (match (op) with
      |Neg -> L.const_int int_t 0   
      |Not -> L.const_int int_t 0   
      )      
  | BinaryOp(op, e1, e2) -> 
    (match op with
      |Add
      |Sub
      |Mult
      |Div
      |Mod  -> L.const_int int_t 0
      |Equal
      |Neq
      |Less
      |Leq
      |Greater
      |Geq  -> L.const_int int_t 0
      |And
      |Or -> L.const_int int_t 0
  )
  | Call(id, args)       -> 
      let f = L.lookup_function id theModule |> Option.get in
      let actuals = List.map (buildExpr env builder) args |> Array.of_list in
      L.build_call f actuals (id^"_result") builder
        
and buildAcc env builder {loc; node} =
  match node with
    |AccVar id -> 
      lookup id env |> Option.get 
    |AccDeref e -> 
      let ptr = buildExpr env builder e in
      let zero = L.const_int int_t 0 in
      L.build_gep ptr [|zero|] "" builder 
    |AccIndex(a, e) -> L.const_int int_t 0

let rec buildStmt env builder {loc; node;} =
  match node with
  | Block list      -> buildBlock env builder list
  | Expr e          -> buildExpr env builder e |> ignore           
  | If(e, s1, s2)   -> 
      ()
  | While(e, s)     -> 
      () 
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
      |Dec (t, id)  -> add_entry id (declareLocalVar (t, id) builder) env
      |Stmt s       -> buildStmt env builder s; env
    ) blockEnv l 
  |> ignore

let buildFunction ({typ; fname; formals; body;} as f) =
    let d = declareFun f in
    let builder = L.builder_at_end theContext (L.entry_block d) in
    let actuals = L.params d |> Array.to_list in
    let actualsEnv = List.fold_left2 
      (allocateParams builder) empty_table formals actuals
    in
    match body.node with 
      |Block l -> buildBlock actualsEnv builder l  
      |_ -> failwith "Illegal AST: function body must be a block"
  

let to_ir (Prog(topdecls)) : L.llmodule =
  List.iter ( 
    fun d -> match d.node with
    |Vardec(t, id)  ->  declareVar (t, id) |> ignore
    |Fundecl f      ->  buildFunction f
  ) topdecls;
  theModule