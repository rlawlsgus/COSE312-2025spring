let main () = 
  let in_c = 
    if Array.length Sys.argv != 2 then 
      raise (Failure "No input is given")
    else 
      Stdlib.open_in (Sys.argv.(1)) in
  let lexbuf = Lexing.from_channel in_c in
  let ast = Parser.program Lexer.next_token lexbuf in
    Eval.eval ast

let _ =  main ()
