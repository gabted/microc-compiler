open Ast
open Symbol_table


let checkFun table f = table

let checkGlobalVar table node = 
  let {loc=l; node=n; id=_} = node in
  match n with
  | Vardec(t, id) ->
    (match t with 
    |TypV -> Util.raise_semantic_error l "Variables cannot be null"
    |_ -> Symbol_table.add_entry id t table)
  | _ -> failwith "Error"


type symbol_info = 
    | FunSignature of typ list * typ
    | VarType of typ
let globalSymbolTable = 
  Symbol_table.empty_table |>
  Symbol_table.add_entry "print" (FunSignature([TypI], TypV)) |>
  Symbol_table.add_entry "getint" (FunSignature([], TypI)) 

  let check (Prog(topdecls)) = 
  (*Accumulates inside "table" the variables and function declaration
    A declaration is added only after checking it's semantic correctness*)
  let _ = List.fold_left ( 
    fun table ann_node -> match getNode ann_node with
      |Fundecl f      -> checkFun table ann_node
      |Vardec(t, i)   -> checkGlobalVar table ann_node
  ) globalSymbolTable topdecls in ()
   
