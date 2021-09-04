/*
* MicroC Parser specification
*/

%{
    open Ast

   
    (*empty statement*)
    let skip = Block [] @> dummy_pos

    (*Used to store information about a variable
    delcaration*)
    type descType = 
      | Id of string 
      | Pointer of descType
      | Array of descType * (int option)
    
    (*Accepts a Ast.typ and a descType, and 
    builds the correct (type, id) pair*)
    let rec buildTypeIdPair t = function 
      |Id s            -> (t, s) 
      | Pointer d      -> let t1, s = buildTypeIdPair t d in
                              (TypP t1, s)
      | Array(d, n) -> let t1, s = buildTypeIdPair t d in
                              (TypA(t1, n), s)

  (* Given two annotated statements s1, s2,
    builds a an annotated block, conceptually equivalent to
    { s1; s2; } *)
  let join_stmts _s1 _s2 = 
    let {loc=p1; node=s1; } = _s1 in
    let {loc=p2; node=s2; } = _s2 in
    let list = [Stmt _s1 @> p1; Stmt _s2 @> p2] in
    Block list @> dummy_pos

  (*builds the AST of the statement
    while(_guard){
      _body;
      _incr;
    }
    if _guard is None, the boolean constant True is used instead
    if _incr is none, nothing is appended at the end of the cycle
  *)
  let buildWhileStmt _guard _incr _body = 
    let guard:expr = 
      Option.value _guard ~default:(BLiteral true @> dummy_pos) in
    let body:stmt =
      match _incr with
      |None -> _body
      |Some e -> join_stmts _body (Expr e @> e.loc) in
    While(guard, body) @> _body.loc
      
%}//header

/* Tokens declarations */
%token IF RETURN ELSE FOR WHILE DO
%token INT CHAR VOID NULL BOOL
%token TRUE FALSE
%token REF PLUS MINUS TIMES DIV REMINDER 
%token ASSIGN MUL_ASSIGN DIV_ASSIGN	MOD_ASSIGN ADD_ASSIGN	SUB_ASSIGN
%token INCR DECR
%token EQ NEQ LESS LEQ GREATER GEQ AND OR NOT
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA SEMI
%token <string>ID
%token <int>LINT
%token <char>LCHAR
%token <string>LSTRING
%token EOF

/* Precedence and associativity specification */
%nonassoc SHORTIF
%nonassoc ELSE
%right   ASSIGN
%left    OR
%left    AND
%left    EQ NEQ
%nonassoc GREATER LESS LEQ GEQ
%left PLUS MINUS
%left TIMES DIV REMINDER
%nonassoc  NOT REF
%nonassoc  LBRACKET

/* Starting symbol */
%start program
%type <Ast.program> program    /* the parser returns a Ast.program value */

%%
/* Grammar specification */

  
program:
  |  l=topDecl* EOF            {Prog(l)}

topDecl:
  | d=varDecl SEMI             {Globaldec d @> $loc}
  | f=funDecl                  {Fundecl f @> $loc}

simpleDec:
  |t=typ d=varDesc { buildTypeIdPair t d}

initDec:
  |t=typ d=varDesc ASSIGN e=expr 
        { let (_t, id) = buildTypeIdPair t d
          in (_t, id, Some(e))}

varDecl:
  |d=simpleDec {(fst d, snd d, None) @> $loc}
  |d=initDec   { d @> $loc}

varDesc:
  |id=ID  {Id id}
  |TIMES d=varDesc  {Pointer d}
  |LPAREN d=varDesc RPAREN {d}
  |d=varDesc LBRACKET RBRACKET {Array(d, None)}
  |d=varDesc LBRACKET n=LINT RBRACKET {Array(d, Some n)}

funDecl:
  |t=typ id=ID LPAREN p=separated_list(COMMA, simpleDec) RPAREN b=block
                  {
                    {typ=t; fname=id; formals=p; body = b }
                  }

block:
  | LBRACE l=stmtOrDec* RBRACE {Block l @> $loc}

stmtOrDec:
  |d=varDecl SEMI           {Localdec d @> $loc}
  |s=statement              {Stmt s @> $loc}

statement:
  | RETURN e=expr? SEMI        {Return e @> $loc}
  | e=expr SEMI                 {Expr e @> $loc}
  | b=block                     {b}
  | WHILE LPAREN e=expr RPAREN s=statement {While(e, s)  @> $loc}
  | DO s=statement WHILE LPAREN e=expr RPAREN SEMI
        {(*desugarin dowhile into while*)
          join_stmts s (While(e, s) @> $loc(s))
        }
  | FOR LPAREN e1=expr? SEMI e2=expr? SEMI e3=expr? RPAREN s=statement
        { (*desugaring of for statement into while*)
          let whileStmt = buildWhileStmt e2 e3 s in
          match e1 with 
            |None ->  whileStmt
            |Some e ->join_stmts (Expr e @> $loc(e1)) whileStmt
        }
  | FOR LPAREN d=varDecl SEMI e2=expr? SEMI e3=expr? RPAREN s=statement
      { (*desugaring of for statement into while
        the declaration and the whilestmt are put into a block,
        limiting the scope of the declared variable*)
        let whileStmt = buildWhileStmt e2 e3 s in
        let list = [Localdec d @> $loc(d); Stmt whileStmt @> $loc(s)] in 
        Block list @> $loc
      }
  | IF LPAREN e=expr RPAREN s1=statement ELSE s2=statement
        {If(e, s1, s2) @> $loc}
  | IF LPAREN e=expr RPAREN s=statement %prec SHORTIF 
        {If(e, s, skip) @> $loc}

expr:
  |e=lExpr  {Access e @> $loc}
  |e=rExpr  {e}

lExpr:
  |id=ID       {AccVar id @> $loc}
  |LPAREN e=lExpr RPAREN {e}
  |TIMES e=aExpr  {AccDeref e @> $loc}
  |TIMES e=lExpr  {AccDeref(Access e @> $loc) @> $loc}
  |e=lExpr LBRACKET i=expr RBRACKET
        {AccIndex(e, i) @> $loc}

rExpr:
  |e=aExpr  {e}
  |id=ID LPAREN l=separated_list(COMMA, expr) RPAREN 
    {Call(id, l) @> $loc}
  |NOT e=expr {UnaryOp(Not, e) @> $loc}
  |MINUS e=expr {UnaryOp(Neg, e) @> $loc}
  |e1=expr op=bin_op e2=expr {BinaryOp(op, e1, e2) @> $loc}
  |le=lExpr ASSIGN v=expr {Assign(le, v) @> $loc}
  |le=lExpr op=trasforming_assign  e=expr %prec ASSIGN
    (*Desugaring of +=, -=, *=, etc 
     x += 1 becomes x += x+1*)
    {let v = BinaryOp(op, (Access(le)@>$loc(le)), e) @> $loc in
      Assign(le, v) @> $loc}
  |e=lExpr INCR {PostIncr e @> $loc}
  |e=lExpr DECR {PostDecr e @> $loc}
  |INCR e=lExpr {PreIncr  e @> $loc}
  |DECR e=lExpr {PreDecr  e @> $loc}
%inline bin_op:
  |PLUS         {Add}
  |MINUS        {Sub} 
  |TIMES        {Mult}
  |DIV          {Div}
  |REMINDER     {Mod}
  |AND          {And}
  |OR           {Or}
  |LESS         {Less}
  |GREATER      {Greater}
  |LEQ          {Leq}
  |GEQ          {Geq}
  |EQ           {Equal}
  |NEQ          {Neq}

trasforming_assign:
  | MUL_ASSIGN  {Mult}
	| DIV_ASSIGN  {Div}
	| MOD_ASSIGN  {Mod}
	| ADD_ASSIGN  {Add}
	| SUB_ASSIGN  {Sub}

aExpr:
  |n=LINT     {ILiteral n @> $loc}
  |c=LCHAR    {CLiteral c @> $loc}
  |s=LSTRING  {SLiteral s @> $loc}
  |TRUE       {BLiteral true @> $loc}
  |FALSE      {BLiteral false @> $loc}
  |NULL       {NullLiteral @> $loc}
  |LPAREN e=rExpr RPAREN {e}
  |REF e=lExpr {Addr e @> $loc}

typ:
  | INT          { TypI     }
  | BOOL         { TypB     }
  | CHAR         { TypC     }
  | VOID         { TypV     }

/*%%
 Trailer */