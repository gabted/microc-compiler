open Ast
open Symbol_table

(*type symbol_info = 
  | FunSignature of typ list * typ
  | VarType of typ*)

type funSignature = typ list * typ
type enviroment = typ t * funSignature t

let rec typeOf env {loc; node;} =
  match node with
    | ILiteral n           -> TypI        
    | CLiteral c           -> TypC       
    | BLiteral b           -> TypB      
    | Access a             -> typeOfAcc env a               
    | Addr a               -> TypP(typeOfAcc env a)     
    | Assign(a, e)         -> 
        (match (typeOfAcc env a, typeOf env e) with
          |TypA _ ,_  -> Util.raise_semantic_error loc 
            "Cannot assign an array"
          |tL, tR when tL <> tR -> Util.raise_semantic_error loc
            "lhs and rhs type discrepancy"  
          |tL, tR -> tL)
    | PostIncr a 
    | PostDecr a 
    | PreIncr  a 
    | PreDecr  a -> if typeOfAcc env a = TypI then TypI
                    else Util.raise_semantic_error loc 
                    "increment must be applied to numeric lvalues"
    | UnaryOp(op, e)       -> 
        (match (op, typeOf env e) with
        |Neg, TypI -> TypI
        |Not, TypB -> TypB
        |Neg, _ -> Util.raise_semantic_error loc 
            "- requires int value"
        |Not, _ -> Util.raise_semantic_error loc 
            "! requires a boolean value"
        )      
    | BinaryOp(op, e1, e2) -> 
      (match (op, typeOf env e1, typeOf env e2) with
        |Add, TypI, TypI
        |Sub, TypI, TypI
        |Mult, TypI, TypI
        |Div, TypI, TypI
        |Mod, TypI, TypI     -> TypI
        |Equal, TypI, TypI
        |Neq, TypI, TypI
        |Less, TypI, TypI
        |Leq, TypI, TypI
        |Greater, TypI, TypI
        |Geq, TypI, TypI     -> TypB
        |And, TypB, TypB
        |Or, TypB, TypB     -> TypB
        |_ -> Util.raise_semantic_error loc 
            "Incorrect operand types"
    )
    | Call(id, args)       -> 
        (match lookup id (snd env) with
          |None -> Util.raise_semantic_error loc
                ("Undeclared function "^id)
          |Some(formalTypes, t) -> 
              let compatible t1 t2 = match t1, t2 with
                |TypA(_t1, None), TypA(_t2, _) -> _t1=_t2
                |_ -> t1 = t2 in
              let actualTypes =  List.map (typeOf env) args in
              if List.equal compatible formalTypes actualTypes 
                then t
                else Util.raise_semantic_error loc 
                  "Invalid actual parameters types"
          )
and typeOfAcc env {loc; node} =
  match node with
    |AccVar id -> 
      (match lookup id (fst env) with
        |None -> Util.raise_semantic_error loc
            ("Undeclared variable "^id)
        |Some  t -> t
        )
    |AccDeref e -> 
        (match typeOf env e with
          |TypP t -> t
          |_ -> Util.raise_semantic_error loc
              ("dereferencing needs a pointer"))
    |AccIndex(a, e) -> 
        let tA = typeOfAcc env a in
        let tI = typeOf env e in
        (match tA, tI with
          |TypA(t, _),TypI -> t (*aggiungere checking statico di Out of BOund?*)
          |TypA _, _ -> Util.raise_semantic_error loc
              ("Array index must be int")
          |_, _ -> Util.raise_semantic_error loc
              ("Cannot acces a non-Array variable"))

let checkVarType loc t = 
  match t with 
  |TypV -> 
      Util.raise_semantic_error loc "Variables cannot be void"
  |TypA(_, Some n) when n<1 -> 
      Util.raise_semantic_error loc "Arrays must have size at least 1"
  |_ -> ()

let addVar loc (env:enviroment) (t, id, v)  = 
  checkVarType loc t;
  if(Option.is_some v && typeOf env (Option.get v) <> t) then
    Util.raise_semantic_error loc "Initializer expression of the wrong type"
  else
    try
      (add_entry id t (fst env), snd env)
    with DuplicateEntry -> 
      Util.raise_semantic_error loc ("Already declared variable: "^id)


let addLocalVar env {loc; node=(t, id, v)} = 
  addVar loc env (t, id, v)

let checkGlobalVar env d = ()

let addGlobalVar env ({loc; node=(t, id, v)} as d) = 
  checkGlobalVar env d; 
  addVar loc env (t, id, v)
    (*TODO: checking delle costanti*)

let rec checkStmt env {loc; node;} returnT= 
  match node with
    | Block list      -> checkBlock env list returnT
    | Expr e          -> ignore (typeOf env e)            
    | If(e, s1, s2)   -> 
        if typeOf env e = TypB then 
            (checkStmt env s1 returnT; checkStmt env s2  returnT)
        else 
          Util.raise_semantic_error loc "Non boolean guard"
    | While(e, s)     -> 
        if typeOf env e = TypB then
             checkStmt env s  returnT
        else 
          Util.raise_semantic_error loc "Non boolean guard"   
    | Return(eOpt)    -> 
        match eOpt,  returnT with
          |None, TypV -> ()
          |Some e, t when typeOf env e = t -> ()
          |_ -> Util.raise_semantic_error loc 
                ("Incorrect return, must be "^show_typ(returnT))

and checkBlock (varT, funT) stmtList returnT=
  ignore( 
    let initialEnv = (begin_block varT, begin_block funT) in
    List.fold_left (
      fun env node -> match node with
        |{loc; node=Localdec d;} -> addLocalVar env d
        |{loc; node=Stmt s;}     -> checkStmt env s returnT; env
      ) initialEnv stmtList
    (*here the end_block operation is omitted since the 
    return  value is ignored*)
  )        


  

  

let checkFun loc env {typ; fname; formals; body}=
  let (varT, funT) = env in
  let checkReturnT = function 
      |TypP _
      |TypA _ -> Util.raise_semantic_error loc "Invalid return type"
      |_ -> ()
  in
    let formalsEnv =  
      let initialEnv = (begin_block varT, begin_block funT) in
      let addFormal env (t, id) = addVar loc env (t, id, None) in
      List.fold_left addFormal initialEnv formals
    in 
    checkReturnT typ;
    match body with 
      |{loc; node=Block l} -> checkBlock formalsEnv l typ 
      |_ -> failwith "Illegal AST: function body must be a block"
    (*here the end_block operation is omitted since the 
    return  value is ()*)
  
let addFun loc env ({typ; fname; formals; body} as f) = 
  let (varT, funT) = env in
    let newEnv = 
      let sign =(List.map fst formals, typ) in
      try
        (varT, add_entry fname sign funT)
      with DuplicateEntry -> 
        Util.raise_semantic_error loc ("Already declared function: "^fname)
    (*Checks the function in the updated enviroment, 
      in order to allow recursive definitions*)
      in checkFun loc newEnv f; newEnv

let globalSymbolTable = 
  let varTable = empty_table in
  let funTable = 
      empty_table |>
      add_entry "print" ([TypI], TypV) |>
      add_entry "getint" ([], TypI)
  in  
  (varTable, funTable)

let check (Prog(topdecls)) = 
  (*Accumulates inside "table" the variables and function declaration
    A declaration is added only after checking it's semantic correctness*)
  let symTable = List.fold_left (
    fun env ann_node -> match ann_node with
    |{loc; node=Globaldec d;}   -> addGlobalVar env d
    |{loc; node=Fundecl f;}       -> addFun loc env f
  ) globalSymbolTable topdecls in
  (*Checks for existance and correctness of the main function*)
  match lookup "main" (snd symTable) with
  |Some ([], TypI)
  |Some ([], TypV) -> ()
  |_ -> Util.raise_semantic_error dummy_pos "Incorrect or absent main function"
   
