
type matrix = float list list;;
type cell= None | Some of float;;
type matrix2= cell list list;;

let _ =
          try
            let lexbuf = Lexing.from_channel stdin in
            while true do
              let result = Parser.main Lexer.token lexbuf in
                 print_newline(); flush stdout
            done
          with Lexer.Eof ->
            exit 0