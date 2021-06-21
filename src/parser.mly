/*
* MicroC Parser specification
*/

%{
    open Ast

    let counter = ref(0)     (* counter to generated unique labels *)
    let next_label () = incr counter; !counter   
    let (@@) node loc = {loc = loc; node = node; id = next_label ()}

    (*
    Given two annotated statements s1, s2,
     build a an annotated block, conceptually equivalent to
     { s1; s2; }
     The statPos of the block is the startPos of s1, and the end is
     the endPos of s2.
     *)
    let join_stmts _s1 _s2 = 
      let {loc=p1; node=s1; id=_} = _s1 in
      let {loc=p2; node=s2; id=_} = _s2 in
      let list = [Stmt _s1 @@ p1; Stmt _s2 @@ p2] in
      Block list @@ (fst p1, snd p2)
    
    let skip = Block [] @@ dummy_pos

    type descType = 
      | Id of string 
      | Pointer of descType
      | Array of descType * (int option)
    
    let rec buildType t = function 
      |Id s            -> (t, s) 
      | Pointer d      -> let t1, s = buildType t d in
                              (TypP t1, s)
      | Array(d, n) -> let t1, s = buildType t d in
                              (TypA(t1, n), s)

  (*int **var -> (TypP(TypP int),var)*)
%}//header

/* Tokens declarations */
%token IF RETURN ELSE FOR WHILE INT CHAR VOID NULL BOOL
%token TRUE FALSE
%token REF PLUS MINUS TIMES DIV REMINDER ASSIGN 
%token EQ NEQ LESS LEQ GREATER GEQ AND OR NOT
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA SEMI
%token <string>ID
%token <int>LINT
%token <char>LCHAR
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
  | d=varDecl SEMI             {Vardec (fst d, snd d) @@ $loc}
  | d=funDecl                  {Fundecl d @@ $loc}

varDecl:
  |t=typ d=varDesc { buildType t d}

varDesc:
  |id=ID  {Id id}
  |TIMES d=varDesc  {Pointer d}
  |LPAREN d=varDesc RPAREN {d}
  |d=varDesc LBRACKET RBRACKET {Array(d, None)}
  |d=varDesc LBRACKET n=LINT RBRACKET {Array(d, Some n)}

funDecl:
  |t=typ id=ID LPAREN p=separated_list(COMMA, varDecl) RPAREN b=block
                  {
                    {typ=t; fname=id; formals=p; body = b }
                  }

block:
  | LBRACE l=stmtOrDec* RBRACE {Block l @@ $loc}

stmtOrDec:
  |d=varDecl SEMI           {Dec (fst d, snd d) @@ $loc}
  |s=statement              {Stmt s  @@ $loc}

statement:
  | RETURN e=expr? SEMI        {Return e @@ $loc}
  | e=expr SEMI                 {Expr e @@ $loc}
  | b=block                     {b}
  | WHILE LPAREN e=expr RPAREN s=statement {While(e, s)  @@ $loc}
  | FOR LPAREN e1=expr? SEMI e2=expr SEMI e3=expr? RPAREN s=statement
            { (*desugaring of for statement into while*)
              let whileStmt = 
                if Option.is_none e3 then 
                  While(e2, s) @@ $loc(s)
                else
                  let incrStatement = Expr(Option.get e3) @@ $loc(e3) in
                  let whileBody = join_stmts s incrStatement in
                  While(e2, whileBody)@@ $loc(s) 
              in
              if Option.is_none e1 then
                  whileStmt
              else 
                join_stmts (Expr(Option.get e1) @@ $loc(e1)) whileStmt
            }
  | IF LPAREN e=expr RPAREN s1=statement ELSE s2=statement
        {If(e, s1, s2) @@ $loc}
  | IF LPAREN e=expr RPAREN s=statement %prec SHORTIF {If(e, s, skip) @@ $loc}

expr:
  |e=lExpr  {Access e @@ $loc}
  |e=rExpr  {e}

lExpr:
  |id=ID       {AccVar id @@ $loc}
  |LPAREN e=lExpr RPAREN {e}
  |TIMES e=aExpr  {AccDeref e @@ $loc}
  |TIMES e=lExpr  {AccDeref(Access e @@ $loc) @@ $loc}
  |e=lExpr LBRACKET i=expr RBRACKET
        {AccIndex(e, i) @@ $loc}

rExpr:
  |e=aExpr  {e}
  |id=ID LPAREN l=separated_list(COMMA, expr) RPAREN 
    {Call(id, l) @@ $loc}
  |le=lExpr ASSIGN v=expr {Assign(le, v) @@ $loc}
  |NOT e=expr {UnaryOp(Not, e) @@ $loc}
  |MINUS e=expr {UnaryOp(Neg, e) @@ $loc}
  |e1=expr op=bin e2=expr {BinaryOp(op, e1, e2) @@ $loc}
%inline bin:
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

aExpr:
  |n=LINT     {ILiteral n @@ $loc}
  |c=LCHAR    {CLiteral c @@ $loc}
  |TRUE       {BLiteral true @@ $loc}
  |FALSE      {BLiteral false @@ $loc}
  |NULL       {ILiteral 0 @@ $loc}
  |LPAREN e=rExpr RPAREN {e}
  |REF e=lExpr {Addr e @@ $loc}

typ:
  | INT          { TypI     }
  | BOOL         { TypB     }
  | CHAR         { TypC     }
  | VOID         { TypV     }

/*%%
 Trailer */