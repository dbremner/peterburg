open Peterburg

module Tree =
  struct

    type t  =
      | Var   of int
      | Const of string
      | Add   of t * t       
      | Assn  of t * t
      | Fetch of t
      |	Loc   of int
      |	Parm  of int
      | Nil
   
    let terminals      = 7
    let degree         = 2
	
    let node = function
      | 0 -> Var   (0)
      | 1 -> Const ("_")
      | 2 -> Add   (Nil, Nil)
      | 3 -> Assn  (Nil, Nil)
      | 4 -> Fetch (Nil)
      |	5 -> Loc   (0)
      |	6 -> Parm  (0)

    let term = function 
      | Var   _ -> 0
      | Const _ -> 1
      | Add   _ -> 2
      | Assn  _ -> 3
      | Fetch _ -> 4
      |	Loc   _ -> 5
      |	Parm  _ -> 6

    let sons = function 
      | Var    _     
      |	Loc    _
      |	Parm   _
      | Const  _     -> []
      | Add   (l, r) -> [l; r]
      | Assn  (l, r) -> [l; r]
      | Fetch (l   ) -> [l]

	    
    let toString = function
      | Var   (n)    -> "var <" ^ (string_of_int n) ^ ">"
      | Const (s)    -> "const <" ^ s ^ ">"
      | Add   (_, _) -> "add"
      | Assn  (_, _) -> "assn"
      | Fetch (_   ) -> "fetch"
      |	Loc   (n)    -> "loc <"  ^ (string_of_int n) ^ ">"
      |	Parm  (n)    -> "parm <" ^ (string_of_int n) ^ ">"
	    
  end

module Nonterms =
  struct

    let nonterms   = [|"IMM"; "REG"; "MEM"; "STACK"|]
    let                 imm ,  reg ,  mem ,  stack    = 0, 1, 2, 3

    let number     = Array.length nonterms
    let toString i = nonterms.(i)

  end

module Attrs =
  struct

(*    type t   = int * loc * int
    and  loc = Symbol of int | Reg of int | Stack of int | Cnst of string 

    let toString (_, loc, _) = 
      match loc with 
      |	Symbol n -> "$" ^ n 
      | Reg    n -> "r" ^ (string_of_int n)
      |	Stack  n -> "stack[" ^ (string_of_int n) ^ "]"
      | Cnst   s -> "%" ^ s

    let cost (c, _, _) = c
*)

    type t = int
	  
    let toString = string_of_int
    let cost x = x
    let (<=) _ _ = 0

  end

module TreeGrammar = Grammar (Tree) (Nonterms) (Attrs);;

open TreeGrammar;;
open Tree;;
open Nonterms;;
open Attrs;;

begin match Verify.run () with
  | Verify.Yes    -> ()
  | Verify.No str -> print_string ("Verification failed: " ^ str ^ ".\n"); exit 255
end;;

module Registers =
  struct

    let num        = 4

    let bit i      = 1 lsl i
    let set i mask = mask lor  (bit i)
    let get i mask = mask land (bit i)
    let clr i mask = mask land lnot (bit i)

    exception Break of int

    let newreg mask =
      try
	for i=0 to num-1 do 
	  if get mask i = 0 then raise (Break i)
	done;
	None
      with
      |	Break i -> Some i

  end

let const = Const ("_")
let var   = Var   (0)
let add   = Add   (Nil, Nil)
let assn  = Assn  (Nil, Nil)
let fetch = Fetch (Nil)
let loc   = Loc   (0)
let parm  = Parm  (0)

let nreg n     = "r" ^ (string_of_int n)
let cnst c     = "%" ^ c
let addr a     = "$" ^ a
let mov  d s   = "\n\tmov\t" ^ d ^ "," ^ s
let indir a    = "[" ^ a ^ "]"
let op   a b c = "\n\t" ^ a ^ "\t" ^ b ^ "," ^ c
let conc a b   = a ^ b

let none _ x _ = x

let balance n m = if n = m then n+1 else max n m

let table = Table.build 
[
 rule (reg, var, [], (fun _ [] -> 1), none), "";
 rule (reg, const, [], (fun _ [] -> 1), none), "";
 rule (reg, fetch, [reg], (fun _ [c] -> c), none), "";
 rule (reg, add, [reg;reg], (fun _ [c;c'] -> balance c c'), none), "";
 rule (reg, assn, [reg; reg], (fun _ [c;c'] -> balance c c'), none), ""
];;

let tree =  
  Add (
  Add (
    Add (
      Fetch (Var (3)), 
      Var (4)
    ),
    Add ( 
      Var (1),
      Var (2)
    )
  ),
  Add (
    Add (
      Var (3), 
      Var (4)
    ),
    Add ( 
      Var (1),
      Var (2)
    )
  ))
;;

match parse table tree with
  | Reducer.Ok   code   -> print_string ("Reduce result: " ^ (string_of_int code) ^ "\n")
  | Reducer.Fail reason -> print_string ("Reduce failed: " ^ reason ^ "\n")

















