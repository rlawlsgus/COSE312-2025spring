%{
%}

%token NEWLINE LPAREN RPAREN PLUS MINUS MULTIPLY DIV POW 
%token <int> NUM

// lowest first
%left PLUS MINUS 
%left MULTIPLY DIV 
%right POW 

%start program
%type <Ast.expr> program
%%

program : exp NEWLINE { $1 }

exp : NUM { Ast.Num ($1) }
| exp PLUS exp { Ast.Add ($1, $3) }
| exp MINUS exp { Ast.Sub ($1, $3) }
| exp MULTIPLY exp { Ast.Mul ($1, $3) }
| exp DIV exp { Ast.Div ($1, $3) }
| exp POW exp { Ast.Pow ($1, $3) }
| LPAREN exp RPAREN { $2 }