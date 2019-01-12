(** The translator 
*   @author Vaibhav Bhandari
* $Id: main.ml,v 1.10 2003/05/09 18:55:27 vaibhav Exp $
*)

(*FIXME output filename*)
let main =
  let input = ref ""  in
  let output = ref "" in
  let inpus s = 
    if (output = ref "") then
      Ass.set_output_file_stream 
	(Str.global_replace (Str.regexp_string ".mv") ".rm" s);
    (input := s) in
  let outpus s =
    output := s; Ass.set_output_file_stream s in
  Arg.parse [("-o", Arg.String(outpus), "Output file");] 
    (inpus) "mv2rm [-o outfile] infile";
  let lex1buf = Lexing.from_channel (open_in !input) in
  let lexbuf = Lexing.from_channel (open_in !input) in
  (*Lexer.make_key_table () ;*)
  try 
    while true do
      Parser.start1 Lexer.token lex1buf;
      Parser.main Lexer.token lexbuf;
    done
  with Lexer.Eof -> exit 0
  | Parsing.Parse_error -> begin
      print_string ("Line " ^ string_of_int(!Lexer.lineno)^": "^
		    (Lexing.lexeme lex1buf) ^ ": Parsing Error\n");
      exit 0;
      end
  | Failure s -> begin
      print_string ("Line " ^ 
		    string_of_int(!Lexer.lineno) ^": " ^ s ^"\n");
      exit 0;
  end;;


  
