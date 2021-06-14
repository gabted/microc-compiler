/*
* MicroC Parser specification
*/

%{
    open Ast

    (* Define here your utility functions *)


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
  |  EOF                      {Prog([])}
;

/*%%*/
/* Trailer */