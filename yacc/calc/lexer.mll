{
  open Parser
  exception LexicalError
}

let number = ['0'-'9']+
let blank = [' ' '\t']

rule token = parse
  | blank { token lexbuf }
  | '\n' { NEWLINE }
  | number { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MULTIPLY }
  | '/' { DIV }
  | '^' { POW }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | _ { raise LexicalError }