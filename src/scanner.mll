{
    open Parser

    let create_hashtable size init =
        let tbl = Hashtbl.create size in
        List.iter (
            fun (key, data) -> Hashtbl.add tbl key data
        ) init;
        tbl


    let keyword_table =
    create_hashtable 14 [
        ("if", IF);
        ("return", RETURN);
        ("else", ELSE);
        ("for", FOR);
        ("while", WHILE);
        ("do", DO);
        ("int", INT);
        ("double", DOUBLE);
        ("char", CHAR);
        ("void", VOID);
        ("NULL", NULL);
        ("bool", BOOL);
        ("true", TRUE);
        ("false", FALSE)
    ]
}(*header*)

(*definitions*)
let digit = ['0' - '9']
let id = ['_' 'a'-'z' 'A'-'Z']['_' 'a'-'z' 'A'-'Z' '0'-'9']*
let ES = '\\'['n' 'r' 't' '\\' '\'' '\"']

(*rules*)
rule token = parse
    digit+ as inum         { let _num = int_of_string_opt inum in
                                match _num with
                                |Some(num) -> LINT(num)
                                |None -> Util.raise_lexer_error lexbuf 
                                        (Lexing.lexeme lexbuf ^": Invalid int format")
                            }
    |digit*'.'digit+ as dnum  {let _num = float_of_string_opt dnum in
                                match _num with
                                |Some(num) -> LDOUBLE(num)
                                |None -> Util.raise_lexer_error lexbuf 
                                        (Lexing.lexeme lexbuf ^": Invalid double format")
                            }
    |"\'\\0\'"                 {LCHAR('\x00')}
    |'\''(ES|['a'-'z' 'A'-'Z' '0' - '9'] as s)'\''    
                             {let _s = Scanf.unescaped s in
                              let c = _s.[0] in
                                LCHAR(c)
                            }
    |'\"'                    {string_parser (Seq.empty) lexbuf}
    | id as word            {try
                                Hashtbl.find keyword_table word
                            with 
                                Not_found ->    ID(word)
                            }
    | '&'                    { REF }
    | '+'                    { PLUS }
    | '-'                    { MINUS }
    | '*'                    { TIMES }
    | '/'                    { DIV }
    | '%'                    { REMINDER }
    | '='                    { ASSIGN }
    | "*="                   { MUL_ASSIGN } 	 	
    | "/="                   { DIV_ASSIGN }
    | "%="                   { MOD_ASSIGN }
    | "+="                   { ADD_ASSIGN }
    | "-="                   { SUB_ASSIGN }
    | "++"                   { INCR }
    | "--"                   { DECR }
    | "=="                   { EQ }
    | "!="                   { NEQ }
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
    | ','                    {COMMA}
    | ';'                    { SEMI }
    | eof                    { EOF }
    | [' ' '\t']             { token lexbuf }
    | '\n'                   { Lexing.new_line lexbuf; token lexbuf }
    | "//"[^ '\n']*'\n'{ Lexing.new_line lexbuf; token lexbuf }
    | "/*"                   {multi_comment lexbuf;}
    | _ as c           { Util.raise_lexer_error lexbuf ("Illegal character " ^ Char.escaped c) }

and string_parser sequence = parse
    |'\"'                                     
        {let s = String.of_seq sequence in
        LSTRING(s)}
    |['a'-'z' 'A'-'Z' '0' - '9' ' ' '\t'] as c          
        {let new_seq = Seq.append sequence (Seq.return c) in
          string_parser new_seq lexbuf}
    |ES as s                                  
         {let escaped = Scanf.unescaped s in
          let c = escaped.[0] in
          let new_seq = Seq.append sequence (Seq.return c) in
          string_parser new_seq lexbuf}
    |"\\0"                                      
        {let new_seq = Seq.append sequence (Seq.return '\x00') in
         string_parser new_seq lexbuf}
    |'\n'                    
        { let new_seq = Seq.append sequence (Seq.return '\n') in
         string_parser new_seq lexbuf}  
    |eof                              
        {Util.raise_lexer_error lexbuf ("Non terminated string")}
    |_ as c 
        {Util.raise_lexer_error lexbuf ("Illegal character whil parsing string" ^ Char.escaped c)}



and multi_comment = parse
     "*/"                     { token lexbuf }
    |'\n'                    { Lexing.new_line lexbuf; multi_comment lexbuf }  
    |eof                     {EOF}
    |_                       { multi_comment lexbuf}