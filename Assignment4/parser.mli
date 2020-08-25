type token =
  | ADD
  | SUBT
  | MULT
  | DIV
  | SUM
  | ROWSUM
  | COLSUM
  | AVG
  | ROWAVG
  | COLAVG
  | MIN
  | ROWMIN
  | COLMIN
  | MAX
  | ROWMAX
  | COLMAX
  | COUNT
  | ROWCOUNT
  | COLCOUNT
  | LPAREN
  | RPAREN
  | LBRAC
  | RBRAC
  | COMMA
  | COLON
  | SEMICOLON
  | ASS
  | INT of (int)
  | FLOAT of (float)
  | EOL

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string
