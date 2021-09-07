/*
* MicroC Parser specification
*/

%{
    open Ast

  (*---Utility declaration and functions 
      used in variable declarations----*) 
  (*Used to store information about type constructors in 
    a variable declaration*)
  type varDesc = 
    | Id of string 
    | Pointer of varDesc
    | Array of varDesc * (int option)
  (*Accepts a Ast.typ and a varDesc, and 
  builds the correct (type, id) pair
  It uses t as an accumulator between recursive calls, 
  so it builds an Ast.typ that is "reversed" wrt the varDesc*)
  let rec buildTypeIdPair t = function 
    | Id s           -> (t, s) 
    | Pointer d      -> 
                  buildTypeIdPair (TypP t) d
    | Array(d, n)    -> 
                  buildTypeIdPair (TypA(t, n)) d

  (*---Utility declarations and functions
    used in stataments -----*)
  (* Given two annotated statements s1, s2,
    builds a an annotated block, conceptually equivalent to
    { s1; s2; } *)
  let join_stmts _s1 _s2 = 
    let {loc=p1; node=s1; } = _s1 in
    let {loc=p2; node=s2; } = _s2 in
    let list = [Stmt _s1 @> p1; Stmt _s2 @> p2] in
    Block list @> dummy_pos
  (*Builds the AST of the statement
      while(_guard){_body;_incr;}
    If _guard is None, the boolean constant True is used instead
    If _incr is none, nothing is appended at the end of the cycle
  *)
  let buildWhileStmt _guard _incr _body = 
    let guard:expr = 
      Option.value _guard ~default:(BLiteral true @> dummy_pos) in
    let body:stmt =
      match _incr with
      |None -> _body
      |Some e -> join_stmts _body (Expr e @> e.loc) in
    While(guard, body) @> _body.loc

  (*empty statement*)
  let skip = Block [] @> dummy_pos      
%}//header

/* Tokens declarations */
%token IF RETURN ELSE FOR WHILE DO
%token INT CHAR VOID NULL BOOL DOUBLE
%token TRUE FALSE
%token REF PLUS MINUS TIMES DIV REMINDER 
%token ASSIGN MUL_ASSIGN DIV_ASSIGN	MOD_ASSIGN ADD_ASSIGN	SUB_ASSIGN
%token INCR DECR
%token EQ NEQ LESS LEQ GREATER GEQ AND OR NOT
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA SEMI
%token <string>ID
%token <int>LINT
%token <float>LDOUBLE
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

/*varDecl is used both in global and local declaration,
and return an Ast.var_decl element, which is an (annotated) triple 
of <typ, id, expr option>*/
varDecl:
  /* initDec returns a triple*/
  |d=initDec                  {d @> $loc}
  /* simpleDec returns a couple*/
  |d=simpleDec                {(fst d, snd d, None) @> $loc}
initDec:
  |t=typ d=varDesc ASSIGN e=expr 
                              {let (_t, id) = buildTypeIdPair t d
                                in (_t, id, Some(e))}
/*simpleDec is used both for variable delcaration 
  and arguments in function delcaration*/
simpleDec:
  |t=typ d=varDesc             {buildTypeIdPair t d}
/*varDesc "wraps" the id into nested type constructos,
  The constructor that is parsed FIRST become the OUTER constructor
  inside varDesc, while it should be the INNER constructor of 
  the Ast.typ. For this reason buildTypeIdPair "reverses" the 
  nested type constructor*/
varDesc:
  |id=ID                              {Id id}
  |TIMES d=varDesc                    {Pointer d}
  |d=varDesc LBRACKET RBRACKET        {Array(d, None)}
  |d=varDesc LBRACKET n=LINT RBRACKET {Array(d, Some n)}
  |LPAREN d=varDesc RPAREN            {d}


funDecl:
  |t=typ id=ID LPAREN p=separated_list(COMMA, simpleDec) RPAREN b=block
                  {{typ=t; fname=id; formals=p; body = b }}
block:
  | LBRACE l=stmtOrDec* RBRACE        {Block l @> $loc}
stmtOrDec:
  |d=varDecl SEMI                     {Localdec d @> $loc}
  |s=statement                        {Stmt s @> $loc}
statement:
  | RETURN e=expr? SEMI               {Return e @> $loc}
  | e=expr SEMI                       {Expr e @> $loc}
  | b=block                           {b}
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
  /*The "if without else" construct is given a precedenc level 
    LOWER than else. By doing so, an "ambiguos" else branch will be 
    associated to the innermost if construct, eliminating 
    shift-reduce conflicts*/
  | IF LPAREN e=expr RPAREN s=statement %prec SHORTIF 
        {If(e, s, skip) @> $loc}

expr:
  |e=lExpr  {Access e @> $loc}
  |e=rExpr  {e}

lExpr:
  |id=ID                 {AccVar id @> $loc}
  |TIMES e=aExpr         {AccDeref e @> $loc}
  |TIMES e=lExpr         {AccDeref(Access e @> $loc) @> $loc}
  |e=lExpr LBRACKET i=expr RBRACKET
                         {AccIndex(e, i) @> $loc}
  |LPAREN e=lExpr RPAREN {e}

rExpr:
  |e=aExpr  {e}
  |id=ID LPAREN l=separated_list(COMMA, expr) RPAREN 
    {Call(id, l) @> $loc}
  |le=lExpr ASSIGN v=expr 
      {Assign(le, v, None) @> $loc} 
  /*modifying assignment operator are not desugared, in order
    to avoid the "duplication" of side effect
    that is present with the naive 
      "int x += n -> int x = x+n"
    desugaring*/
  |le=lExpr op=ass_op v=expr %prec ASSIGN
                      {Assign(le, v, Some(op)) @> $loc} 
  |NOT e=expr         {UnaryOp(Not, e) @> $loc}
  |MINUS e=expr       {UnaryOp(Neg, e) @> $loc}
  |e=lExpr INCR       {PostIncr e @> $loc}
  |e=lExpr DECR       {PostDecr e @> $loc}
  |INCR e=lExpr       {PreIncr  e @> $loc}
  |DECR e=lExpr       {PreDecr  e @> $loc}
  |e1=expr op=bin_op e2=expr 
                      {BinaryOp(op, e1, e2) @> $loc}
/*operators are inlined, so that the defined precedences
  resolve automatically shift-reduce conflicts.*/
%inline bin_op:
  |PLUS                {Add}
  |MINUS               {Sub} 
  |TIMES               {Mult}
  |DIV                 {Div}
  |REMINDER            {Mod}
  |AND                 {And}
  |OR                  {Or}
  |LESS                {Less}
  |GREATER             {Greater}
  |LEQ                 {Leq}
  |GEQ                 {Geq}
  |EQ                  {Equal}
  |NEQ                 {Neq}
/*assignment operators don't have to be inlined, since
  they all uses the ASSIGN level of precedence*/
ass_op:
  |MUL_ASSIGN          {Mult}
  |DIV_ASSIGN          {Div}
  |MOD_ASSIGN          {Mod}
  |ADD_ASSIGN          {Add}
  |SUB_ASSIGN          {Sub}

aExpr:
  |n=LINT              {ILiteral n @> $loc}
  |c=LCHAR             {CLiteral c @> $loc}
  |s=LSTRING           {SLiteral s @> $loc}
  |d=LDOUBLE           {DLiteral d @> $loc}
  |TRUE                {BLiteral true @> $loc}
  |FALSE               {BLiteral false @> $loc}
  |NULL                {NullLiteral @> $loc}
  |REF e=lExpr         {Addr e @> $loc}
  |LPAREN e=rExpr RPAREN {e}

typ:
  | INT          { TypI     }
  | DOUBLE       { TypD     }
  | BOOL         { TypB     }
  | CHAR         { TypC     }
  | VOID         { TypV     }

/*%%
 Trailer */