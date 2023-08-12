(*******************************************************************************************************)
(*                                                                                                     *)
(*                                                                                                     *)
(*  Generic BURS (Bottom-Up Rewriting System) module.                                                  *)
(*                                                                                                     *)
(*  (C) St.Petersburg State University, Russia, 2001                                                   *)
(*                                                                                                     *)
(*                                                                                                     *)
(*******************************************************************************************************)

(* Module type to abstract tree being rewrited                                                         *)

module type Tree =
  sig
    
    type t                             (* Abstract tree type                                           *)

    val terminals : int                (* Number of terminals                                          *)   
    val degree    : int                (* Degree                                                       *)
    val term      : t -> int           (* Function to access teminal of node                           *)
    val node      : int -> t           (* Function to convert terminal to node (for debugging)         *)
    val sons      : t -> t list        (* Function to obtain list of sons                              *)
    val toString  : t -> string        (* Function to produce string representation of node            *)
                                       (* (without subtree rooted by this node)                        *)    

  end

(* Module type to abstract set of nonterminals                                                         *)

module type Nonterminal =
  sig 

    val number   : int                 (* Number of nonterminals of tree grammar                       *)
    val toString : int -> string       (* String representation of nonterminal                         *)

  end

(* Module type to abstract attribute holded by nonterminal during labeling procedure                   *)

module type Attribute =           
  sig 

    type t                             (* Abstract attribute type                                      *)

    val cost     : t -> int            (* Function to extract cost                                     *)
    val (<=)     : t -> t -> int       (* Comparison operator to control order of reductions           *)
    val toString : t -> string         (* String representation of attribute                           *)

  end

val nosort : 'a -> 'b -> int           (* Fake function to represent no need in sorting                *)

(* Functor to instantiate BURS grammar                                                                 *)

module Grammar :
  functor (Tree    : Tree       ) ->   (* Tree to cover                                                *)
  functor (Nonterm : Nonterminal) ->   (* Set of nonterminals                                          *)
  functor (Attr    : Attribute  ) ->   (* Labeling attribute                                           *)
  sig

    type   nonterm = int
    and    term    = int
    and    sema    = Tree.t -> Attr.t list -> Attr.t          (* Labeling semantic action              *)
    and 'a rema    = Tree.t -> Attr.t      -> 'a list -> 'a   (* Reduce semantic action                *)
    and    predic  = Tree.t -> Attr.t list -> bool            (* Auxilliary predicate to check rule's  *)
    and 'a core                                               (* applicability                         *)
    and 'a rule    = 'a core * string                         (* Rule and it's label (for debugging)   *)

    (* Miscellaneous functions to construct rules                                                      *)

    val rule     : nonterm * Tree.t * nonterm list          * sema * 'a rema -> 'a core
    val rule_p   : nonterm * Tree.t * nonterm list * predic * sema * 'a rema -> 'a core

    val chain    : nonterm * nonterm          * sema * 'a rema -> 'a core
    val chain_p  : nonterm * nonterm * predic * sema * 'a rema -> 'a core

    val toString : 'a rule -> string                          (* String representation of rule         *)

    (* Module to represent parsing table                                                               *)

    module Table :
      sig

        type 'a t                                             (* Table type                            *)
        and  'a rc = Ok of 'a t | Fail of string              (* Result of table construction          *)
	      
        val build    : 'a rule list -> 'a rc                  (* Build function                        *)
        val toString : 'a rc        -> string                 (* String representation of table        *)
	      
      end

    (* Module to represent labeler                                                                     *)

    module Labeler :
      sig

        exception Nomatch of Tree.t                           (* Exception to obtain non-derived node  *)

        type 'a t                                             (* Labeling result                       *)
	and  'a labels                                        (* Set of all derived tree labels        *)
        and  'a rc = Ok of 'a t | Fail of string              (* Labeler return code                   *)

        val toString : 'a rc  -> string                       (* String representation of covering     *)
        val run      : Tree.t -> 'a Table.rc -> 'a rc         (* Labeler entry point                   *)

      end

    (* Module to represent reducer                                                                     *)

    module Reducer :
      sig

        type 'a rc = Ok of 'a | Fail of string                (* Reducer return code                   *)

        val toString : 'a rc -> string                        (* Representation of reducer result      *)
        val run      : 'a Labeler.rc -> 'a rc                 (* Reducer entry point                   *)

      end
	
    (* Module to verify modules being used as parameters                                               *)

    module Verify :
      sig 

	type t = Yes | No of string                           (* Result of verification                *)

	val run : unit -> t                                   (* Verificator entry point               *)

      end

    (* BURS main entry point                                                                           *)

    val parse      : 'a Table.rc -> Tree.t -> 'a Reducer.rc

    (* Useful functions for tree traversing                                                            *)

    val fold_left  : ('a -> Tree.t -> 'a) -> Tree.t -> 'a -> 'a
    val fold_right : ('a -> Tree.t -> 'a) -> Tree.t -> 'a -> 'a

  end

(* Module for conventional BURS (e.g. without labeling attributes)                                     *)

module BURS :
  functor (Tree    : Tree       ) ->   
  functor (Nonterm : Nonterminal) ->   
  sig

    type   nonterm = int
    and    term    = int
    and    cost    = Tree.t -> int list -> int
    and 'a rema    = Tree.t -> 'a list  -> 'a  
    and    predic  = Tree.t -> int list -> bool           
    and 'a core                                              
    and 'a rule    = 'a core * string                        

    val rule     : nonterm * Tree.t * nonterm list          * cost * 'a rema -> 'a core
    val rule_p   : nonterm * Tree.t * nonterm list * predic * cost * 'a rema -> 'a core

    val chain    : nonterm * nonterm          * cost * 'a rema -> 'a core
    val chain_p  : nonterm * nonterm * predic * cost * 'a rema -> 'a core

    val toString : 'a rule -> string                         

    module Table :
      sig

        type 'a t                                            
        and  'a rc = Ok of 'a t | Fail of string             
	      
        val build    : 'a rule list -> 'a rc                 
        val toString : 'a rc        -> string                
	      
      end

    module Labeler :
      sig

        exception Nomatch of Tree.t                           

        type 'a t                                             
        and  'a rc = Ok of 'a t | Fail of string              

        val toString : 'a rc  -> string                       
        val run      : Tree.t -> 'a Table.rc -> 'a rc         

      end

    module Reducer :
      sig

        type 'a rc = Ok of 'a | Fail of string               

        val toString : 'a rc -> string                       
        val run      : 'a Labeler.rc -> 'a rc                

      end

    module Verify :
      sig 

	type t = Yes | No of string                          

	val run : unit -> t                                  

      end

    val parse      : 'a Table.rc -> Tree.t -> 'a Reducer.rc

    val fold_left  : ('a -> Tree.t -> 'a) -> Tree.t -> 'a -> 'a
    val fold_right : ('a -> Tree.t -> 'a) -> Tree.t -> 'a -> 'a

  end

(* Module for conventional BURS with constant costs                                                    *)

module ConstBURS :
  functor (Tree    : Tree       ) ->   
  functor (Nonterm : Nonterminal) ->   
  sig

    type   nonterm = int
    and    term    = int
    and    cost    = int
    and 'a rema    = Tree.t -> 'a list  -> 'a  
    and 'a core                                              
    and 'a rule    = 'a core * string                        

    val rule     : nonterm * Tree.t  * nonterm list          * cost * 'a rema -> 'a core
    val chain    : nonterm * nonterm                         * cost * 'a rema -> 'a core

    val toString : 'a rule -> string                         

    module Table :
      sig

        type 'a t                                            
        and  'a rc = Ok of 'a t | Fail of string             
	      
        val build    : 'a rule list -> 'a rc                 
        val toString : 'a rc        -> string                
	      
      end

    module Labeler :
      sig

        exception Nomatch of Tree.t                           

        type 'a t                                             
        and  'a rc = Ok of 'a t | Fail of string              

        val toString : 'a rc  -> string                       
        val run      : Tree.t -> 'a Table.rc -> 'a rc         

      end

    module Reducer :
      sig

        type 'a rc = Ok of 'a | Fail of string               

        val toString : 'a rc -> string                       
        val run      : 'a Labeler.rc -> 'a rc                

      end

    module Verify :
      sig 

	type t = Yes | No of string                          

	val run : unit -> t                                  

      end

    val parse      : 'a Table.rc -> Tree.t -> 'a Reducer.rc

    val fold_left  : ('a -> Tree.t -> 'a) -> Tree.t -> 'a -> 'a
    val fold_right : ('a -> Tree.t -> 'a) -> Tree.t -> 'a -> 'a

  end














































