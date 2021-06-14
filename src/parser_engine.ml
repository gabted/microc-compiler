module I =
  Parser.MenhirInterpreter


let parse lexbuf =
  let supplier = I.lexer_lexbuf_to_supplier Scanner.token lexbuf in
  let start = Parser.Incremental.program lexbuf.lex_curr_p in
  I.loop supplier start

