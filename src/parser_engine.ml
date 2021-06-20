open Util

let parse lexbuf =
  try
    Parser.program Scanner.token lexbuf
  with
    Parser.Error -> raise_syntax_error lexbuf ("Syntax Error on lexeme:"^ Lexing.lexeme lexbuf)