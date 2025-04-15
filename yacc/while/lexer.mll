{
  open Parser
  exception LexingError of string

  let kwd_list : (string * Parser.token) list =
    [ 
      ("true", BOOLEAN true);
      ("false", BOOLEAN false);
      ("if", IF);
      ("else", ELSE);
      ("while", WHILE);
      ("skip", SKIP); 
      ("print", PRINT) 
    ]

  let id_or_kwd (s : string) : Parser.token =
    match List.assoc_opt s kwd_list with
    | Some t -> t
    | None -> IDENT s
}

let letter    = ['a'-'z' 'A'-'Z']
let digit     = ['0'-'9']
let number    = digit+
let space     = ' ' | '\t' | '\r'
let blank     = space+
let new_line  = '\n' | "\r\n"
let ident     = letter (letter | digit | '_')*

let comment_line_header   = "//"

rule next_token = parse
  | comment_line_header   { comment_line lexbuf }
  | blank                 { next_token lexbuf }
  | new_line              { Lexing.new_line lexbuf; next_token lexbuf }
  | ident as s            { id_or_kwd s }
  | number as n           { NUMBER (int_of_string n) }
  | '('                   { LPAREN }
  | ')'                   { RPAREN }
  | '{'                   { LBRACE }
  | '}'                   { RBRACE }
  | ';'                   { SEMICOLON }
  | "=="                  { EQ }
  | "<="                  { LE }
  | '!'                   { NOT }
  | '+'                   { PLUS }
  | '-'                   { MINUS }
  | '*'                   { STAR }
  | '/'                   { SLASH }
  | '%'                   { MOD }
  | "&&"                  { BAND }
  | ":="                  { ASSIGN }
  | eof                   { EOF }
  | _ as c                { LexingError (": illegal character \'" ^ (c |> String.make 1) ^ "\'") |> Stdlib.raise }

and comment_line = parse
  | new_line              { Lexing.new_line lexbuf; next_token lexbuf }
  | eof                   { EOF }
  | _                     { comment_line lexbuf }