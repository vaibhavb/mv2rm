// (* $Id: parser.mly,v 1.28 2003/07/01 00:19:56 vaibhav Exp $ *)
%{
open Ass
open Ast
open Codegen
%}

/* File parser.mly */
%token TokModel TokEnd TokRoot
%token TokInputs TokOutputs
%token TokMV TokTable TokDef TokReset TokLatch
%token TokEOL TokEOF
%token TokTabDef
%token <string> TokTabVal
%token <string> TokVal
%token <string> TokTabVar
%token TokTabEnd
%token TokSubckt
%token <string> TokVar
%token TokNo
%token COMMA ARROW HYPHEN ASSIGN LPAREN RPAREN LBRACE RBRACE NOT
%start start1
%type <unit> start1
%start main
%type <unit> main
%%

/*
* Very Informal Documentation
* BlifMV ::= model
* model ::= Inputs Outputs Commands
*/

/* 1st parser is start1 */

start1:
    models1 TokEOF { (*print_string("parse1\n");  print_all_models();*)}
;

models1:
     model1 models1 {}
     | {}
;

model1:
      head1 decl1 body1 TokEnd TokEOL
      { add_model_to_modelTab ();
	add_subcktTab ();
        clear_all_list ();
      }
;

head1:
    TokModel TokVar TokEOL
      { set_module_name ($2);
      }
;
    
decl1:
      vdecl1 {}
  | {}      
;
    
vdecl1:
      vdecl1 input1{}
  | input1 {}
  | vdecl1 output1{}
  | output1 {}
;

input1:
      TokInputs iovals1 TokEOL 
      { add_i_list ($2); }
;

output1:
      TokOutputs iovals1 TokEOL 
      { add_o_list ($2); }
;

iovals1:
      TokVar iovals1 {List.append [$1] ($2)}
  | TokVar {[$1]}
;

body1:
   /*(* TokNo body {}*): Give lexical error*/
   table1 body1 {}
  | subckt1 body1 {}
  | mv1  body1 {}
  | reset1  body1 {}
  | latch1 body1 {}
  | {}
;  

/*Handling tables*/
table1:
      TokTable tabvars1 TokEOL relations1  {}
;
    
/*Handling MV*/
mv1:
      TokMV mvvars1 TokVal values1 TokEOL 
      {}
;

mvvars1:
      TokVar COMMA mvvars1 {}
  | TokVar {}

values1:
      TokVar values1 {}
  |TokVal  values1 {}
  | {}
;

/*Handling Resets*/
reset1:
      TokReset tabvars1 TokEOL relations1  {}
;

/*Handling Subckt*/
subckt1:
      TokSubckt TokVar TokVar param_passing1 TokEOL 
        {make_subckt ($2) ($3) ($4);
	  add_sckt_list ($2);}
;

param_passing1:
    formal_actual1 param_passing1 {make_pair_list ($1) ($2)}
  | {(List.combine [] [])}
;
formal_actual1:
      TokVar ASSIGN TokVar 
      { (List.combine [$1] [$3])}
;

/*Handling Latches*/
latch1:
      TokLatch TokVar TokVar TokEOL { }
;

/*Table and Reset Latch Handlers*/
tabvars1:
      inList1 ARROW outList1 { }
  |   TokTabVar inList1  { }
  | TokTabVar  { }
;

inList1:
    TokTabVar inList1 {}
  | TokTabVar {}
  | {} //(*FIXME not sure*)
;

outList1:
    TokTabVar outList1 {}
  | TokTabVar {}
;

relations1:
      vdefault relations1 { }
  | relation1 relations1 {}
  | {}
;

vdefault1:
      TokTabDef valList1 TokEOL {}
;

relation1:
      valList1 TokEOL {}
;
    
valList1:
      valb1 valList1 { }
  | valb1 { }
;

valb1:
      TokTabVal {}
  |   TokTabVar {}
  | ASSIGN  TokTabVar {}
  | HYPHEN  {}
  | LBRACE TokTabVal HYPHEN TokTabVal RBRACE {}
  | LPAREN vals1 RPAREN {}
  | NOT valb1 {}
;      

vals1:
      TokTabVal COMMA vals1 {}
  | TokTabVal {}
;


/* second parser */

main:
    models TokEOF
     { print_string("parse2\n");
       emit_license();
       emit_all_subckts(); 
       exit 0
     }
;

models:
    model models {}
  | {}
;

model:
      head decl body TokEnd TokEOL
      { add_hidelist();
        emit_module_name (); 
	emit_io_stmts (); clear_io_list ();
        emit_private_vars(); 
	emit_atoms(); 
	emit_module_end ();
	(*add_subcktTab ();*)
	clear_subckt(); 
      }
;

head:
    TokModel TokVar TokEOL
      { clear_all_list (); 
	set_module_name ($2);
      (*to take care of type gadbad*) }
;
    
decl:
      vdecl {}
  | {}      
;
    
vdecl:
      vdecl input{}
  | input {}
  | vdecl output{}
  | output {}
;

input:
      TokInputs iovals TokEOL 
      { add_i_list ($2); List.iter add_i_sym_tab ($2) }
;

output:
      TokOutputs iovals TokEOL 
      { add_o_list ($2); List.iter add_o_sym_tab ($2) }
;

iovals:
      TokVar iovals {List.append [$1] ($2)}
  | TokVar {[$1]}
;

body:
   /*(* TokNo body {}*): Give lexical error*/
   table body {}
  | subckt body {}
  | mv  body {}
  | reset  body {}
  | latch body {}
  | {}
;  

/*Handling tables*/
table:
      TokTable tabvars TokEOL relations  
      { 
	add_tableatom_to_symTab ();
	clear_relations ();
	clear_inout_list ();
      }
;
    
/*Handling MV*/
mv:
      TokMV mvvars TokVal values TokEOL 
      { 
	let add_in a = 
	  match ($4) with
	    []-> add_sym_tab a (MvRange(int_of_string($3)))
	  | _ -> 
	      if (List.length ($4) == int_of_string($3)) then
		add_sym_tab a (Mv([$3]@($4)))
	      else
		raise (Failure (a ^" has unmatched number of values"));
	in
	List.iter add_in ($2) 
      }
;

mvvars:
      TokVar COMMA mvvars {List.append [$1] ($3)}
  | TokVar {[$1]}

values:
      TokVar values {List.append [$1] ($2)}
  | TokVal values {List.append [$1] ($2)}
  | {[]}
;

/*Handling Resets*/
reset:
      TokReset tabvars TokEOL relations  
      { add_resetatom_to_symTab ();
        clear_relations ();
        clear_inout_list ();
      }
;

/*Handling Subckt*/
subckt:
      TokSubckt TokVar TokVar param_passing TokEOL 
       { io_analysis ($2) ($3) ($4);
        make_subckt ($2) ($3) ($4) ;}
;

param_passing:
    formal_actual param_passing {make_pair_list ($1) ($2)}
  | {(List.combine [] [])}
;
formal_actual:
      TokVar ASSIGN TokVar 
      { (*interface_analysis ($3);*) (List.combine [$1] [$3]);}
        
;

/*Handling Latches*/
latch:
      TokLatch TokVar TokVar TokEOL 
      { add_sym_tab ($2) (Bool);add_sym_tab ($3) (Bool);
	add_latchatom_to_symTab ($2) ($3);
      }
;

/*Table and Reset Latch Handlers*/
tabvars:
      inList ARROW outList { add_in_list ($1); 
			     add_out_list ($3)}
  |   TokTabVar inList  
      { add_sym_tab ($1) (Bool);
	add_in_list  ([$1]@( List.rev (List.tl (List.rev $2)))); 
    	add_out_list ([(List.hd (List.rev ($2)) )])
      }
  | TokTabVar  { add_sym_tab ($1) (Bool); add_out_list ([$1]); }
;

inList:
    TokTabVar inList {add_sym_tab ($1) (Bool); List.append [$1] ($2)}
  | TokTabVar {add_sym_tab ($1) (Bool); [$1]}
  | {[]} //(*FIXME not sure*)
;

outList:
    TokTabVar outList {add_sym_tab ($1) (Bool); List.append [$1] ($2) }
  | TokTabVar {add_sym_tab ($1) (Bool); [$1]}
;

relations:
      vdefault relations { add_def_relation (); }
  | relation relations { add_relation ($1); }
  | {}
;

vdefault:
      TokTabDef valList TokEOL { make_def_relation ($2) }
;

relation:
      valList TokEOL { ($1) }
;
    
valList:
      valb valList { ([$1]@($2)) }
  | valb { [$1] }
;

valb:
      TokTabVal { Cmp(($1)) }
  |   TokTabVar { Cmp(($1)) }
  | ASSIGN  TokTabVar {(*check it in io*)  Cmp(($2)^"'") }
  | HYPHEN  { Hyp }
  | LBRACE TokTabVal HYPHEN TokTabVal RBRACE { Range(($2),($4)) }
  | LPAREN vals RPAREN { Set(($2)) }
  | NOT valb { Not(($2)) }
;      

vals:
      TokTabVal COMMA vals {([$1]@($3))}
  | TokTabVal {([$1])}
;


%%









