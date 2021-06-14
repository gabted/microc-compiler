/*
* MicroC Parser specification
*/

%{
    open Ast

    (* Define here your utility functions *)
    let (@@) node loc = {loc = loc; node = node; id = 0}

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
  |  l=topDecl* EOF                      {Prog(l)}

topDecl:
  | d=funDecl                {Fundecl d @@ $loc}
  | d=varDecl SEMI             {Vardec (fst d, snd d) @@ $loc}

varDecl:
  |t=typ id=ID {(t, id)}

funDecl:
  |t=typ id=ID LPAREN p=separated_list(COMMA, varDecl) RPAREN
                  {
                    {typ=t; fname=id; formals=p; body = Return None @@ dummy_pos}
                  }

typ:
  | INT          { TypI     }
  | BOOL         { TypB     }
  | CHAR         { TypC     }
  | VOID         { TypV     }

/*%%
 Trailer */