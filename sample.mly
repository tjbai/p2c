%token <int> INT
%token PLUS MINUS TIMES DIV LPAREN RPAREN

%left PLUS MINUS
%left TIMES DIV

%start <int> expr

%%

expr:
  | INT { $1 }
  | expr PLUS expr { $1 + $3 }
  | expr MINUS expr { $1 - $3 }
  | expr TIMES expr { $1 * $3 }
  | expr DIV expr { $1 / $3 }
  | LPAREN expr RPAREN { $2 }
