{ }

rule token = parse
  | "(*"[^ \n]*'\n'                    { print_endline "one line comments"; token lexbuf }
  | [' ' '\t' '\n']         { token lexbuf }
  | ['a'-'z']+ as word      { Printf.printf "word: %s\n" word; token lexbuf }
  | _ as c                  { Printf.printf "char %c\n" c; token lexbuf }
  | eof                     { raise End_of_file }

{
let () =
  let lexbuf = Lexing.from_channel stdin in
  try
    while true do
      token lexbuf
    done
  with End_of_file -> ()
}