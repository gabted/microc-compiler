{
    open Parser

    let create_hashtable size init =
        let tbl = Hashtbl.create size in
        List.iter (
            fun (key, data) -> Hashtbl.add tbl key data
        ) init;
        tbl


    let keyword_table =
    create_hashtable 8 [
        ("if", IF);
        ("return", RETURN);
        ("else", ELSE);
        ("for", FOR);
        ("while", WHILE);
        ("int", INT);
        ("char", CHAR);
        ("void", VOID);
        ("NULL", NULL);
        ("bool", BOOL);
        ("true", TRUE);
        ("false", FALSE)
    ]
}
    let digit = ['0' - '9']
    let id = ['_' 'a'-'z' 'A'-'Z']['_' 'a'-'z' '0'-'9']*


rule token = parse
    digit+ as inum         { let num = int_of_string inum in
                                LINT(num)
                            }
    |'\''['a'-'z' 'A'-'Z' '0' - '9']'\'' as s   
                             {let c = s.[1] in
                                LCHAR(c)
                            }
    | id as word            {try
                                Hashtbl.find keyword_table word
                            with Not_found ->
                                ID(word)
                            }
    | '+'                    { PLUS }
    | '-'                    { MINUS }
    | '*'                    { TIMES }
    | '/'                    { DIV }
    | '%'                    { REMINDER }
    | '='                    { EQ }
    | "=="                   { DEQ }
    | "!="                    { NEQ }
    | '<'                    { LESS }
    | "<="                   { LEQ }
    | '>'                    { GREATER }
    | ">="                   { GEQ }
    | "&&"                   { AND }
    | "||"                   { OR }
    | '!'                   { NOT }
    | '('                    { LPAREN }
    | ')'                    { RPAREN }
    | '{'                    { LBRACE}
    | '}'                    { RBRACE }
    | '['                    { LBRACKET }
    | ']'                    { RBRACKET }
    | [' ' '\t']             { token lexbuf }
    | '\n'                   { Lexing.new_line lexbuf; token lexbuf }
    | eof                    { EOF }
    | _ as c           { Util.raise_lexer_error lexbuf ("Illegal character " ^ Char.escaped c) }
