{

    open Parser
    exception Eof
    exception Invalid_Input   
    type matrix = float list list 
}
    

rule token = parse
    | "ADD"          {ADD}
    | "SUBT"         {SUBT}
    | "MULT"         {MULT}
    | "DIV"          {DIV}
    | "COUNT"        {COUNT}
    | "ROWCOUNT"     {ROWCOUNT}
    | "COLCOUNT"     {COLCOUNT}
    | "SUM"          {SUM}
    | "ROWSUM"       {ROWSUM}
    | "COLSUM"       {COLSUM}
    | "AVG"          {AVG}
    | "ROWAVG"       {ROWAVG}
    | "COLAVG"       {COLAVG}
    | "MIN"          {MIN} 
    | "ROWMIN"       {ROWMIN}
    | "COLMIN"       {COLMIN}
    | "MAX"          {MAX}
    | "ROWMAX"       {ROWMAX}
    | "COLMAX"       {COLMAX}
    | "("            {LPAREN}
    | ")"            {RPAREN}
    | "["            {LBRAC}
    | "]"            {RBRAC}
    | ","            {COMMA}
    | ":"            {COLON}
    | ";"            {SEMICOLON}
    | ":="           {ASS}
    | ['-''+']?(['1'-'9']['0'-'9']*|['0'])['.'](['0'-'9']*['1'-'9']|['0']) as lx {FLOAT(float_of_string lx)}
    | ['-''+']?(['0']|['1'-'9']['0'-'9']*) as lm {INT(int_of_string lm)}
    | [' ' '\t']          {token lexbuf}
    | ['\n']          {token lexbuf}
    | eof            {raise Eof}
    | _              {raise Invalid_Input}

    