/*
* MicroC Parser specification
*/

%{
    open Ast

    (* Define here your utility functions *)
    
    let (@@) node loc = {loc = loc; node = node; id = 0}

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

%}//header

/* Tokens declarations */
%token IF RETURN ELSE FOR WHILE INT CHAR VOID NULL BOOL
%token TRUE FALSE
%token DEREF PLUS MINUS TIMES DIV REMINDER ASSIGN 
%token EQ NEQ LESS LEQ GREATER GEQ AND OR NOT
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA SEMI
%token <string>ID
%token <int>LINT
%token <char>LCHAR
%token EOF

/* Precedence and associativity specification */
%right   EQ
%left    OR
%left    AND
%left    DEQ NEQ
%nonassoc GREATER LESS LEQ GEQ
%left PLUS MINUS
%left TIMES DIV REMINDER
%nonassoc  NOT DEREF
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
  |t=typ id=ID {(t, id)}

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
              else join_stmts (Expr(Option.get e1) @@ $loc(e1)) whileStmt
            }
  | IF LPAREN e=expr RPAREN s=statement {If(e, s, s) @@ $loc}

expr:
  |n=LINT     {ILiteral n  @@ $loc}

typ:
  | INT          { TypI     }
  | BOOL         { TypB     }
  | CHAR         { TypC     }
  | VOID         { TypV     }

/*%%
 Trailer */