%{
  open Ast
%}

%token <string> IDENT
%token <int> NUMBER
%token <bool> BOOLEAN

%token LPAREN RPAREN LBRACE RBRACE SEMICOLON EOF
%token BAND NOT LE EQ PLUS MINUS STAR SLASH MOD ASSIGN SKIP PRINT IF ELSE WHILE 

// lowest first
%left   BAND 
%left   EQ
%left   LE
%left   PLUS MINUS
%left   STAR SLASH MOD
%right  NOT            

%type <Ast.cmd> cmd 
%type <Ast.aexp> aexp 
%type <Ast.bexp> bexp 
%type <Ast.program> program 

%start program

%% 
// production rules

// program : cmd_seq EOF  { $1 }

// cmd_seq : 
// | cmd { $1 }
// | cmd_seq cmd { Seq ($1, $2) }

// cmd : IDENT ASSIGN aexp SEMICOLON { Assign ($1, $3) }
// | SKIP SEMICOLON { Ast.Skip }
// | IF LPAREN bexp RPAREN LBRACE cmd_seq RBRACE ELSE LBRACE cmd_seq RBRACE { If ($3, $6, $10) }
// | WHILE LPAREN bexp RPAREN LBRACE cmd_seq RBRACE { While ($3, $6) }
// | PRINT LPAREN aexp RPAREN SEMICOLON { Print $3 }

program : cmd EOF  { $1 }

cmd : IDENT ASSIGN aexp SEMICOLON { Assign ($1, $3) }
| SKIP SEMICOLON { Ast.Skip }
| cmd cmd { Ast.Seq ($1, $2) }
| IF LPAREN bexp RPAREN LBRACE cmd RBRACE ELSE LBRACE cmd RBRACE { If ($3, $6, $10) }
| WHILE LPAREN bexp RPAREN LBRACE cmd RBRACE { While ($3, $6) }
| PRINT LPAREN aexp RPAREN SEMICOLON { Print $3 }


aexp : NUMBER { Int $1 }
| IDENT { Var $1 }
| aexp PLUS aexp { Add ($1, $3) }
| aexp MINUS aexp { Sub ($1, $3) }
| aexp STAR aexp { Mul ($1, $3) }
| aexp MOD aexp { Mod ($1, $3) }
| LPAREN aexp RPAREN { $2 }

bexp : BOOLEAN { Bool $1 }
| aexp EQ aexp { Eq ($1, $3) }
| aexp LE aexp { Le ($1, $3) }
| NOT bexp { Neg $2 }
| bexp BAND bexp { Conj ($1, $3) }
| LPAREN bexp RPAREN { $2 }

