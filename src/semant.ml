open Ast
open Symbol_table

type symbol_info = 
    | FunSignature of typ list * typ
    | VarType of typ

let checkVar table (t, id) loc = 
    match t with 
        |TypV -> 
            Util.raise_semantic_error loc "Variables cannot be void"
        |TypA(_, Some n) when n<1 -> 
            Util.raise_semantic_error loc "Arrays must have size at least 1"
        |_ -> ()

let addVar loc table (t, id) = 
  checkVar table (t, id) loc;
  match lookup id table with
  |None ->  add_entry id (VarType t) table
  |Some(_) -> 
    Util.raise_semantic_error loc "Already declared variable"

let addStmt loc env = function
    | If(_, _, _)     -> env
    | While(e, s)     -> env       
    | Expr e          -> env            
    | Return(eOpt)    -> env          
    | Block list      -> env 

let addBlock loc stmtList env =
  let bodyEnv = 
    let bodyEnv = begin_block env in
    List.fold_left (
      fun env ann_node -> match ann_node with
        |{loc; node=Dec(t, id); } -> addVar loc env (t, id)
        |{loc; node=Stmt {loc=_loc; node=s}; }     -> addStmt _loc env s
     ) bodyEnv stmtList |> end_block 
  in bodyEnv
  

let checkFun loc table {typ; fname; formals; body}=
  let formalsEnv =  
    let formalsEnv = begin_block table in
    List.fold_left (addVar loc) formalsEnv formals
    |> end_block
  in 
  let _ = match body with 
    |{loc; node=Block(list)} -> addBlock loc list formalsEnv
    |_ -> failwith "Illegal AST: function body must be a block"
  in ()
  
let buildFunSignature {typ; fname; formals; body} =
  FunSignature(List.map fst formals, typ)
  
let addFun loc table f = 
  checkFun loc table f;
  let {typ; fname=id; formals; body} = f in
  match lookup id table with
  |None ->  add_entry id (buildFunSignature f) table
  |Some(_) -> 
    Util.raise_semantic_error loc ("Already declared function: "^id) 

let globalSymbolTable = 
  empty_table |>
  add_entry "print" (FunSignature([TypI], TypV)) |>
  add_entry "getint" (FunSignature([], TypI)) 

let check (Prog(topdecls)) = 
  (*Accumulates inside "table" the variables and function declaration
    A declaration is added only after checking it's semantic correctness*)
  let symTable = List.fold_left (
    fun env ann_node -> match ann_node with
    |{loc=l; node=Fundecl f; }      -> addFun l env f
    |{loc=l; node=Vardec(t, i); }   -> addVar l env (t, i)
  ) globalSymbolTable topdecls in
  (*Checks for existance and correctness of the main function*)
  match lookup "main" symTable with
  |Some FunSignature([], TypI)
  |Some FunSignature([], TypV) -> ()
  |_ -> Util.raise_semantic_error dummy_pos "Incorrect or absent main function"
   
