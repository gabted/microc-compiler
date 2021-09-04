type binop = Add | Sub | Mult | Div  | Mod | Equal | Neq | Less | Leq | 
             Greater | Geq | And | Or
             [@@deriving show]

type uop = Neg | Not [@@deriving show]

type identifier = string [@@deriving show]

type position = Lexing.position * Lexing.position 
let dummy_pos = (Lexing.dummy_pos, Lexing.dummy_pos) 


type 'a annotated_node = {loc : position[@opaque]; node : 'a; }[@@deriving show]
let (@>) node loc = {loc = loc; node = node;} 

type typ =
  | TypI                             (* Type int                    *)
  | TypB                             (* Type bool                   *)
  | TypC                             (* Type char                   *)
  | TypA of typ * int option         (* Array type                  *)
  | TypP of typ                      (* Pointer type                *)
  | TypNullP                         (* NullPointer type                *)
  | TypV                             (* Type void                   *)
  [@@deriving show]

and expr =  expr_node annotated_node                                                   
and expr_node =     
  | Access of access                 (* x    or  *p    or  a[e]     *) 
  | Assign of access * expr          (* x=e  or  *p=e  or  a[e]=e   *)
  | PostIncr of access                   (* x++  or  *p++  or  a[e]++   *)
  | PostDecr of access                   (* x--  or  *p--  or  a[e]--   *)
  | PreIncr of access                   (* ++x  or  ++*p  or  ++a[e]   *)
  | PreDecr of access                   (* --x  or  --*p  or  --a[e]   *)
  | Addr of access                   (* &x   or  &*p   or  &a[e]    *)
  | ILiteral of int                  (* Integer literal             *)
  | CLiteral of char                 (* Char literal                *)
  | BLiteral of bool                 (* Bool literal                *)
  | SLiteral of string               (* String literal              *)
  | NullLiteral                      (* NULL *)
  | UnaryOp of uop * expr            (* Unary primitive operator    *)
  | BinaryOp of binop * expr * expr  (* Binary primitive operator   *)
  | Call of identifier * expr list   (* Function call f(...)        *)
  [@@deriving show]

and access = access_node annotated_node
and access_node =                                                       
  | AccVar of identifier             (* Variable access        x    *) 
  | AccDeref of expr                 (* Pointer dereferencing  *p   *)
  | AccIndex of access * expr        (* Array indexing         a[e] *)
  [@@deriving show]

and stmt = stmt_node annotated_node
and stmt_node =                                                         
  | If of expr * stmt * stmt         (* Conditional                 *)
  | While of expr * stmt             (* While loop                  *)
  | Expr of expr                     (* Expression statement   e;   *)
  | Return of expr option            (* Return statement            *)
  | Block of stmtordec list          (* Block: grouping and scope   *)
  [@@deriving show]

and stmtordec = stmtordec_node annotated_node
and stmtordec_node =                                                    
  | Localdec of var_decl          (* Local variable declaration  *)
  | Stmt of stmt                     (* A statement                 *)
  [@@deriving show]

and var_decl = var_decl_node annotated_node
and var_decl_node = typ * identifier *(expr option)

type fun_decl = {
  typ : typ;
  fname : string;
  formals : (typ*identifier) list;
  body : stmt;
}[@@deriving show]

type topdecl = topdecl_node annotated_node
and topdecl_node = 
  | Fundecl of fun_decl
  | Globaldec of var_decl
  [@@deriving show]

type program = Prog of topdecl list [@@deriving show]