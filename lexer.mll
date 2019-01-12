(* The BlifMV lexer *)
(* $Id: lexer.mll,v 1.14 2003/10/27 21:44:06 vaibhav Exp $ *)
{
open Parser (* tokens are defined in parser.mly *)
exception Eof
let isTable = ref false
let diagnostic s = () (* begin print_string (s ^ "..\n"); flush stdout 
end *)

(*The idea to replace is by combination of & FIXME code*)
let replace s = begin
  let ft = ref true in
  let n = ref ""  in
  let f c = 
    let str = String.make 1 c in
    match str with
      "$" ->  if (!ft) then (n := ((!n)^ "a&"); ft:=false )
      else n := ((!n)^ "&" )
    | "_" -> if (!ft) then (n := ((!n)^ "a&&"); ft:=false )
    else n := ((!n)^ "&&" )
    | "<" -> if (!ft) then (n := ((!n)^ "a&&&"); ft:=false )
    else n := ((!n)^ "&&&" )
    | ">" -> if (!ft) then (n := ((!n)^ "a&&&&"); ft:=false )
    else n := ((!n)^ "&&&&" )
    | "?" -> if (!ft) then (n := ((!n)^ "a&&&&&"); ft:=false )
    else n := ((!n)^ "&&&&&" )
    | "@" -> if (!ft) then (n := ((!n)^ "a&&&&&&"); ft:=false )
    else n := ((!n)^ "&&&&&&" )
    | "|" -> if (!ft) then (n := ((!n)^ "a&&&&&&&"); ft:=false )
    else n := ((!n)^ "&&&&&&&" )
    | "+" -> if (!ft) then (n := ((!n)^ "a&&&&&&&&"); ft:=false )
    else n := ((!n)^ "&&&&&&&&" )
    | "*" -> if (!ft) then (n := ((!n)^ "a&&&&&&&&&"); ft:=false )
    else n := ((!n)^ "&&&&&&&&&" )
    | _ ->   if (!ft) then (n := ((!n) ^ str); ft :=false)
    else n := ((!n) ^ str )
  in
  String.iter f s;
  !n
end

let lineno = ref 0
let incr_ll () =
  lineno := !lineno + 1      
let countLines c =
  let str = String.make 1 c in
  match str with 
    "\n" -> lineno:=!lineno+1
  | _ -> ()

let keyword_table = Hashtbl.create 10
let _ =
    let add_entry a =
        Hashtbl.add keyword_table a (a^"_a") in		
    List.iter add_entry
              [ "update";"controls";"atom"]	
}

(** 
*   .model
*   .inputs
*   .outputs
*   .commands (.mv .names .r .def .latch .subckt)
*   .end
*)

let blanks = [' ' '\t' '\r']+
let blank = [' ' '\t' '\r']*
let dotKeyWord = ['.']['a'-'z' 'A'-'Z']* 
let id_normal = ['a'-'z' 'A'-'Z' '0'-'9']+ 
let id_special = ['a'-'z' 'A'-'Z' '0'-'9' '$' '<' '>' '_' '?' '|' '+' '*' '@']+ let digit = ['0'-'9']+

    rule token = parse
   blanks {token lexbuf}
| '\n' {incr_ll ();diagnostic "NewLine"; TokEOL}
| ['\n']+ blank ['\n']+  {String.iter countLines (Lexing.lexeme lexbuf);
			   diagnostic "NewLine"; TokEOL}
| '#' { comment lexbuf}
| dotKeyWord
  {
  match Lexing.lexeme lexbuf with
    ".model" -> diagnostic "Module" ; isTable := false; TokModel
  | ".root" -> TokRoot
  | ".end" -> diagnostic "End"; isTable := false; TokEnd
  | ".inputs" -> diagnostic "Inputs"; TokInputs
  | ".outputs" -> diagnostic "Outputs"; TokOutputs
  | ".names" | ".table" -> diagnostic "Tables"; isTable := true; TokTable
  | ".def"|".default" -> (
      if !isTable then
	(diagnostic "Table Default";TokTabDef)
      else
	(diagnostic "Not Table Default";TokDef)
	  )
  | ".r"|".reset" -> diagnostic "Reset"; isTable := true; TokReset
  | ".mv" -> diagnostic "MV"; isTable := false; TokMV
  | ".subckt" -> diagnostic "Subckt"; isTable := false; TokSubckt
  | ".latch" -> diagnostic "Latch"; isTable := false; TokLatch
  | _ -> raise (Failure "Not a Keyword, InValid identifier")
}
 
| digit   { 
      if !isTable then 
	(diagnostic "Table Number"; TokTabVal(Lexing.lexeme lexbuf))
      else
	(diagnostic "Number"; TokVal(Lexing.lexeme lexbuf))
  } 
| id_special   {
  if !isTable then
    (diagnostic "Not a KeyWord"; 
    try
       TokTabVar((Hashtbl.find keyword_table (Lexing.lexeme lexbuf))	     )
    with Not_found ->
       TokTabVar(replace(Lexing.lexeme lexbuf))
    )
  else
    (diagnostic "Not a table KeyWord"; 
    try
       TokVar((Hashtbl.find keyword_table (Lexing.lexeme lexbuf))	     )
    with Not_found ->
       TokVar(replace(Lexing.lexeme lexbuf))
    )
} 
| id_normal   {
  if !isTable then
    (diagnostic "Not a KeyWord"; 
     try
       TokTabVar(Hashtbl.find keyword_table (Lexing.lexeme lexbuf))	     
     with Not_found ->
       TokTabVar((Lexing.lexeme lexbuf))
     )
  else
    (diagnostic "Not a table KeyWord"; 
     try
       TokVar(Hashtbl.find keyword_table (Lexing.lexeme lexbuf))
     with Not_found ->       
       TokVar((Lexing.lexeme lexbuf))
    )
} 

| "->" {ARROW}
| '-' {HYPHEN}
| '=' {ASSIGN}
| ','  {COMMA}
| '{' {LBRACE}
| '}' {RBRACE}
| '(' {LPAREN}
| ')' {RPAREN}
| '!' {NOT}
| _ {raise (Failure "unrecognized token")
            (*diagnostic "Unrec"; TokNo*)} (* match any character *)
| eof {TokEOF}    
    
and comment = parse
    ['\n']+ { String.iter countLines (Lexing.lexeme lexbuf);
	      token lexbuf }
| _  { comment lexbuf }
| eof {raise Eof}

    
(*
and newLine = parse
   '\n' {incr_ll (); newLine lexbuf}
   | blanks {newLine lexbuf}
   |  _ { token lexbuf}
   
   
and table = parse
   ".names" { diagnostic "Tables"; TokTable }
   |".def" { diagnostic "Table Default"; TokTabDef; table lexbuf}
   | ['0'-'9'] {TokTabVal(int_of_string (Lexing.lexeme lexbuf)); table lexbuf}
   | tabVar { diagnostic "Not a KeyWord"; 
   TokTabVar(Lexing.lexeme lexbuf); table lexbuf}
| "->" {TokTabPt;table lexbuf}
| '#' {comment lexbuf}
| "??" {TokTabEnd; token lexbuf}
| _ {token lexbuf}
*)



