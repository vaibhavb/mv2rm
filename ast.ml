(* Abstract Syntax Tree of a module*)
(* $Id: ast.ml,v 1.17 2003/07/01 00:19:56 vaibhav Exp $ *)
let refiList = ref [""]
let refoList = ref [""]
let refsList = ref [""]
let refhideList = ref [""]
let modelList = ref[""]

(* table structure *)
let refinList = ref [""]
let refoutList = ref [""]

let unmarked = ref [""]

type relation = Cmp of string | Hyp | Range of string*string | Set of string list | Not of relation | True | False | Default
let def_relation:relation list ref = ref []
let relation:relation list ref= ref []
let relations:relation list list ref= ref [([])]

type form = Input | Output | Private
type typeVal  = Bool | Mv of string list | MvRange of int
type read = Reads of string
type control = Controls of string list
type lcontrol = LControls of string
type rcontrol = RControls of string list
type await = Awaits of string list
type relations = Relations of relation list list
type atom = None | ResetAtom of control * await * relations
                 | TableAtom of control * await * relations
                 | LatchAtom of lcontrol * rcontrol * read
		       * await * relations 
		 | SameAs of string
type entry = Empty | Symb of form * typeVal * atom

(* ashwini *)
type modelEntry = {
    inlist : string list ;
    outlist : string list ;
    slist : string list ;
    mutable hlist : string list;
  }
	
let make_new_model ilist olist sl = {
  inlist = !ilist;
  outlist = !olist ;
  slist =  !sl ;
  hlist = [];
  }

let add_hidelist_to_model model hideList = 
  model.hlist <- hideList;;
 
let get_hidelist model =
  model.hlist;;


(** symbol table
    symbol -> List of values it can take
              [] if Boolean
*)
let symTab = Hashtbl.create 50
let init_tab = Hashtbl.add symTab "" Empty

(* subckts *)
let subcktList = ref [([("","")])]
let subcktTab = Hashtbl.create 25
(* FIXME : Cleaner design *)
let init_subcktTab = Hashtbl.add subcktTab "" [([("","")])]

let out_file_stream =  ref (open_out "prog.rm")

let moduleName = ref ""

(* modelTab is a hash table of models, hashed by the modelname
  and containing the 3 lists, inlist, outlist, subcktlist of the 
  model *)
(*let modelTab = Hashtbl.create 17
let init_modelTab = Hashtbl.add modelTab "" ([""],[""], [""])*)

