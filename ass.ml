(* $Id: ass.ml,v 1.33 2003/08/11 18:24:57 ashwini Exp $ *)
open Ast

let set_module_name s =
  moduleName := s

let modelTab = Hashtbl.create 17

(* ashwini *)
let add_model_list s = 
  modelList := (!modelList)@(s)

let add_sckt_list s = 
  refsList := (!refsList) @ [(s)]

let add_hide_list s = 
  refhideList := (!refhideList) @ [(s)]

let add_i_list s = 
  refiList := (!refiList)@(s)

let add_o_list s =
  refoList := (!refoList)@(s)
			     
let clear_io_list () = 
  refiList:=[]; refoList:=[]
      
let add_in_list s = 
  refinList := (!refinList)@(s)
			       
let clear_inout_list () = 
  refinList:=[];refoutList:=[]
      
let clear_def_relation () = 
  def_relation := []

let make_def_relation s = 
  def_relation := (!def_relation)@(s)

let add_def_relation () =
  relations := (!relations) @ ([[Default]@(!def_relation)])

let add_relation s = 
  relations := (!relations) @ [(s)]
				
(* To get rid of typing stupidity *)
let clear_all_list () =
  refiList:=[]; refoList:=[]; refsList:=[]; refhideList :=[];
  refinList:=[];refoutList:=[];
  def_relation := []; relation := [];
  relations := []; subcktList := [];
  Hashtbl.clear symTab;
  unmarked := []
      
let clear_relations () =
  relations := []

let clear_relation () =
  relation := []

let clear_subckt () =
  subcktList := []

let make_pair_list a b = 
  List.combine (List.append (fst (List.split a)) (fst (List.split b))) (List.append (snd (List.split a)) (snd (List.split b)))

let make_subckt name instance params =
  let head = List.combine [name] [instance] in
  subcktList := (!subcktList) @ [(make_pair_list head params)]

(*Operations on Symbol Table
  Add symbol valueList (take care if already present)
  Search symbol valueList 
  Clear use Hashtbl.clear symTab
*)
let search_sym_tab symbol =
  Hashtbl.mem symTab symbol
    
let add_sym_tab symbol symbVal =
  match symbVal with
    Bool ->  
      if ((search_sym_tab symbol)=false) then 
	Hashtbl.add symTab symbol (Symb(Private, Bool, None))
  | _ ->    
      begin
	try 
	  begin
	    match (Hashtbl.find symTab symbol) with
	      Symb(Input, Bool, _) -> 
		Hashtbl.replace symTab symbol (Symb(Input,symbVal, None))
	    | Symb(Output, Bool, _)-> 
		Hashtbl.replace symTab symbol (Symb(Output,symbVal, None))
	    | _ -> raise (Failure("Illegal MV Operation"))	  
	  end
	with Not_found ->
	  Hashtbl.replace symTab symbol (Symb(Private,symbVal, None))
      end
	

(* If the variable is private, make it Input. If it is an output
  var also make it an input var, for it is now being input to the
  a&module from the subckt, and will be output in module = a&module 
  || subckt*)
let add_i_sym_tab symbol =
    (*print_endline ("trying to input symbol to symTab "^symbol);*)
  try begin
    let oldSymbol = Hashtbl.find symTab symbol in
    match (oldSymbol) with
      Symb(Private,Bool,None) ->
        Hashtbl.replace symTab symbol (Symb(Input, Bool, None))
    | Symb(Private,Mv(valuelist),None) ->
        Hashtbl.replace symTab symbol (Symb(Input, Mv(valuelist), None))
    | Symb(Private,MvRange(int),None) ->
        Hashtbl.replace symTab symbol (Symb(Input, MvRange(int), None))
    | Symb(Output,a,b) ->
        Hashtbl.replace symTab symbol (Symb(Input, a, b))
    | _ ->
        raise (Failure ("Illegal Input Variable "^symbol))
  end
  with Not_found ->
    (*print_endline ("adding input symbol to symTab "^symbol);*)
    Hashtbl.add symTab symbol (Symb(Input, Bool, None))
        
let add_o_sym_tab symbol =
  try begin
    let oldSymbol = Hashtbl.find symTab symbol in
    match (oldSymbol) with
      Symb(Private, a, b) ->
        Hashtbl.replace symTab symbol (Symb(Output, a, b))
    | _ ->
        raise (Failure ("Illegal Output Variable "^symbol))
  end
  with Not_found ->
    (*print_endline ("adding output symbol to symTab "^symbol);*)
    Hashtbl.add symTab symbol (Symb(Output, Bool, None))

let set_output_file_stream s =
  out_file_stream := open_out s
    
let markTable s =
  if (search_sym_tab s) then begin
    match (Hashtbl.find symTab s) with
      Symb(a, b, None) ->
	Hashtbl.replace symTab s (
	Symb(a,b,TableAtom(
	     Controls(!refoutList),
	     Awaits(!refinList),
	     Relations(!relations)
	       )))
    | _ ->  raise (Failure("Already Controlled :"^s))
  end
      
let add_out_list s = 
  refoutList := (!refoutList)@(s)
				 
let add_subcktTab () =
  if ((Hashtbl.mem subcktTab !moduleName)) then ()
  else
    Hashtbl.add subcktTab !moduleName (!subcktList)

let clear_subcktTab () =
  Hashtbl.clear subcktTab

let add_tableatom_to_symTab () =
  match (!refoutList) with
    hd::tl -> 
      markTable hd;
      let mark s =
	if (search_sym_tab s) then begin
	  match (Hashtbl.find symTab s) with
	    Symb(a, b, None) ->
	      Hashtbl.replace symTab s (Symb(a,b,SameAs(hd)))
	  | _ ->  raise (Failure("Already Controlled :"^s))
	end in
      List.iter mark tl	
  | _ -> raise (Failure("There has to be atleast one output variable"))


let markReset s =
  if (search_sym_tab s) then begin
    match (Hashtbl.find symTab s) with
      Symb(a, b, None) ->
	Hashtbl.replace symTab s (
	Symb(a,b,ResetAtom(
	     Controls(!refoutList),
	     Awaits(!refinList),
	     Relations(!relations)
	       )))
    |Symb(a,b,LatchAtom(
	     LControls(s),
	     RControls(p),
	     Reads(input),
	     Awaits(q),
	     Relations(r))) ->
	       	Hashtbl.replace symTab s (
	Symb(a,b,LatchAtom(
	     LControls(s),
	     RControls(!refoutList),
	     Reads(input),
	     Awaits(!refinList),
	     Relations(!relations)
	    )))
    | _ ->  raise (Failure("Already Controlled :"^s))
  end
      
let add_resetatom_to_symTab () =
  match (!refoutList) with
    hd::tl -> 
      markReset hd;
      let mark s =
	if (search_sym_tab s) then begin
	  match (Hashtbl.find symTab s) with
	    Symb(a, b, None) ->
	      Hashtbl.replace symTab s (Symb(a,b,SameAs(hd)))
	  | _ ->  raise (Failure("Already Controlled :"^s))
	end in
      List.iter mark tl	
  | _ -> raise (Failure("There has to be atleast one output variable"))


let add_latchatom_to_symTab input s =
  if (search_sym_tab s) then begin
    match (Hashtbl.find symTab s) with
      Symb(a, b, ResetAtom(
	   Controls(p),
	   Awaits(q),
	   Relations(r)
	     ))->
	Hashtbl.replace symTab s (
	Symb(a,b,LatchAtom(
	     LControls(s),
	     RControls(p),
	     Reads(input),
	     Awaits(q),
	     Relations(r)
	       )))
    | Symb(a, b, SameAs(other)) -> begin
	match (Hashtbl.find symTab other) with
	  Symb(a, b, ResetAtom(
	       Controls(p),
	       Awaits(q),
	       Relations(r)
		 )) ->
		   Hashtbl.replace symTab other (
		   Symb(a,b,LatchAtom(
			LControls(s),
			RControls(p),
			Reads(input),
			Awaits(q),
			Relations(r)
		       )))
	| _ ->  raise (Failure("A latch should be intialized "^s))
    end
    | Symb(a, b, None) ->
	Hashtbl.replace symTab s (
	Symb(a,b,LatchAtom(
	     LControls(s),
	     RControls([]),
	     Reads(input),
	     Awaits([]),
	     Relations([])
	    )))
    | _ ->  raise (Failure("A latch should be intialized "^s))
  end  

let interface_analysis symbol = 
  if (search_sym_tab symbol) then begin
    match (Hashtbl.find symTab symbol) with
      Symb(Output, b,c) -> begin
	Hashtbl.replace symTab symbol (Symb(Input,b,c));
	let equalz s = (symbol = s) in
	refoList := snd(List.partition equalz (!refoList));
	refiList := (!refiList)@[symbol]
      end
|Symb(Private, b,c) -> begin
    Hashtbl.replace symTab symbol (Symb(Input,b,c));
        refiList := (!refiList)@[symbol]
end
    | Symb(_,_,_) -> ()
    | _ ->  raise (Failure("Unknown parameter: "^symbol))

  end

let print_list lst =
  List.iter ( fun var ->
    print_endline ( var )
	     ) lst
    

let is_output_variable subckt var =
  (*print_endline("subckt "^subckt^" has the var "^var);*)
  try 
    let model =  Hashtbl.find modelTab subckt in
    if(  List.mem var model.outlist) then true
    else false
  with Not_found ->
    raise (Failure("Not a valid subckt: "^subckt))


(* Given a list of subckts of a module, find if a given variable
is the output of any subckt of that module 
First find if the var is an actual parameter of any subckt.
If it is, then check if it is the formal paramter associated with 
that actual parameter is an output of that subckt.
Function returns bool : true if not an output of any subckt.
  			false  if output of a subckt*)
let rec not_found_instantiation var instantiatedSlist subckt = 
  match (instantiatedSlist) with
    [] -> true
    | hd :: tl ->
      let subcktAName = ( List.hd(snd(List.split hd))) in
      let subcktFName = ( List.hd(fst(List.split hd))) in
      if ((subcktAName = "") || (subcktAName = subckt))  then begin
        (*print_endline ("empty subcktlist or ");*)
        not_found_instantiation var tl subckt
      end
      else begin
        (*print_endline("searching subckt "^subcktAName);*)
        let actualVarsList = (snd(List.split hd)) in
	if( List.mem var actualVarsList) then begin
	  (* reverse the assiciations of the list, to get the
	  formal par, given the actual par *)
	  let formalVarsList = (fst(List.split hd)) in 
	  let reversedAssocList = List.combine actualVarsList formalVarsList in
          let formalVar = List.assoc var reversedAssocList in
          (*print_endline ("found formal var "^ formalVar );*)
	  if( is_output_variable subcktFName formalVar) then false
	  else begin
	    not_found_instantiation var tl subckt
	  end
	end
	else begin
	  not_found_instantiation var tl subckt
	end
      end


let add_to_hidelist var =
  if( List.mem var (!refhideList)) then ()
  else begin
    refhideList := (!refhideList)@[var]
  end

let add_to_refoList_and_symTab var =
  if( List.mem var (!refoList)) then () 
  else begin
    (*print_endline ( "  added to refoList and refhideList: "^var);*)
    (*add_to_hidelist var;
    refoList := (!refoList)@[var]
    add_o_sym_tab var;*)
    try begin
      let oldSymbol = Hashtbl.find symTab var in
      match (oldSymbol) with
        Symb(Private, a, b) -> 
	  (* make the private var of the module, an o/p var and
	  * also add it to the hideList to be hidden
	  * after composition *)
          add_to_hidelist var;
	  Hashtbl.replace symTab var (Symb(Output, a, b));
          refoList := (!refoList)@[var]
       | Symb(Input,a,b) ->
         Hashtbl.replace symTab var (Symb(Input, a, b))
       | _ ->
         raise (Failure ("Illegal Output Variable "^var))
     end
     with Not_found ->
       add_to_hidelist var;
       Hashtbl.add symTab var (Symb(Output, Bool, None));
       refoList := (!refoList)@[var]
   end
  
let add_to_refiList_and_symTab var =
  if( List.mem var (!refiList)) then () 
  else begin
    if( List.mem var (!refoList)) then begin
      (* the var is output by the subckt and the also the module.
         So add it to the refiList of the a&module and remove it from the 
	 refoList of a&module. Also do not hide it in the
	 composition of module := a&module || subckt. So module effectively
	 outputs the variable.*)
      let newolist = List.filter ( fun element ->
                 element <> var ) (!refoList) in
      refoList := newolist;
      refiList := (!refiList)@ [var];
      add_i_sym_tab var;
    end
    else begin
      add_to_hidelist var;
      add_i_sym_tab var;
      refiList := (!refiList)@ [var]
    end
  end

let print_model m =
  List.iter ( fun var ->
  	print_string ( var^"\n " )
	) m.inlist;
  List.iter ( fun var ->
  	print_string ( var^" \n" )
	) m.outlist;
  List.iter ( fun var ->
  	print_string ( var^" \n" )
	) m.slist

let print_all_models () =
  Hashtbl.iter (fun key value1 -> 
  	print_string ("Model is "^key^"\n") ;
	print_model value1;
  	(*List.iter print_string ("%s\n")  value1.inlist;*)
	
	) modelTab

let print_hash key =
  print_string ("model name" ^ !key ^" \n")
  
(*FIXME: Ashwini's output FIX *)
let io_analysis subcktFormal subcktActual varPairList =
  (*print_string ("\n"^ !moduleName ^ " is the modulename \n");*)
  try
    let model = Hashtbl.find modelTab subcktFormal in
    try 
      let instantiatedSlist = Hashtbl.find subcktTab !moduleName in
      List.iter (fun pair ->
        if( List.mem (fst(pair)) model.outlist) then begin
	  (* if formal parameter is an output of subckt, make
	     the actual par.  an input of the module *)
	  add_to_refiList_and_symTab (snd(pair));
	end
        else if (List.mem (fst(pair)) model.inlist ) then begin 
	  (* the formal parameter is an input of subckt *)
	  (* find if the formal parameter is an output of another
	    subckt of the model *)
	  if ( not_found_instantiation (snd(pair)) instantiatedSlist 
	       subcktActual ) 
	  then begin
	    (* Not an output var of another subckt *)
	    add_to_refoList_and_symTab (snd(pair));
	  end
        end
	  ) varPairList;
    with Not_found ->
      raise (Failure("Not a valid module "^ !moduleName))
  with Not_found ->
    raise (Failure("Not a valid subckt "^subcktFormal))


let search_model_tab m =
  Hashtbl.mem modelTab m


let add_model_to_modelTab () =
  if (search_model_tab (!moduleName) ) then begin
    raise (Failure "Duplicate model name")
  end else begin
    let model = Ast.make_new_model refiList refoList refsList in
      Hashtbl.add modelTab !moduleName (model);
      (*Hashtbl.add modelTab !moduleName ( !refiList, !refoList, !refsList);*)
			(*!refoList,
			!subcktList)*)
  end
  
let add_hidelist () =
  if (List.length (!refhideList) != 0) then 
    let oldmodel = Hashtbl.find modelTab !moduleName in
    Ast.add_hidelist_to_model oldmodel (!refhideList)


