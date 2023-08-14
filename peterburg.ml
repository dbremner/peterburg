let rec fold_int i n c f = if i < n then fold_int (i + 1) n (f i c) f else c
let void _ = ()
let nosort _ _ = 0

module type Tree = sig
  type t

  val terminals : int
  val degree : int
  val term : t -> int
  val node : int -> t
  val sons : t -> t list
  val toString : t -> string
end

module type Nonterminal = sig
  val number : int
  val toString : int -> string
end

module type Attribute = sig
  type t

  val cost : t -> int
  val ( <= ) : t -> t -> int
  val toString : t -> string
end

module Grammar (Tree : Tree) (Nonterm : Nonterminal) (Attr : Attribute) = struct
  module WrappedNonterm (Nonterms : Nonterminal) = struct
    let number = Nonterms.number
    let toString = Nonterms.toString

    module Verify = struct
      type t = Yes | Crash of string

      let run () =
        try
          let _ =
            fold_int 0 number () (fun i _ -> void (Nonterms.toString i))
          in
          Yes
        with x -> Crash (Printexc.to_string x)

      let toString = function
        | Yes -> "ok"
        | Crash n -> "exception \"" ^ n ^ "\" raised"
    end
  end

  module WrappedTree (PTree : Tree) = struct
    type t = Foo of PTree.t * int * t list

    let build t =
      let rec inner t id =
        let sons, id =
          List.fold_left
            (fun (curr, id) t ->
              let (Foo (_, id', _) as t'), _ = inner t id in
              (t' :: curr, id' + 1))
            ([], id) (PTree.sons t)
        in
        (Foo (t, id, List.rev sons), id + 1)
      in
      inner t 0

    let toString' (Foo (t, i, _)) = string_of_int i ^ ": " ^ PTree.toString t

    let toString tree =
      let rec inner ofs (Foo (_, _, sons) as tree) =
        String.make ofs ' ' ^ toString' tree
        ^ List.fold_left (fun curr t -> curr ^ "\n" ^ inner (ofs + 2) t) "" sons
      in
      inner 0 tree

    let terminals = PTree.terminals
    let degree = PTree.degree
    let node i = Foo (PTree.node i, -1, [])
    let term (Foo (t, _, _)) = PTree.term t
    let sons (Foo (_, _, s)) = s
    let id (Foo (_, i, _)) = i
    let orig (Foo (t, _, _)) = t

    module Verify = struct
      type t = Yes | No of (string * int) list | Crash of string

      let run () =
        try
          match
            fold_int 0 PTree.terminals [] (fun i c ->
                if i <> PTree.term (PTree.node i) then
                  (PTree.toString (PTree.node i), i) :: c
                else c)
          with
          | [] -> Yes
          | x -> No x
        with x -> Crash (Printexc.to_string x)

      let toString = function
        | Yes -> "ok"
        | Crash name -> "exception \"" ^ name ^ "\" raised"
        | No lst ->
            "unmatched pairs found: ("
            ^ List.fold_left
                (fun curr (s, i) ->
                  (if curr = "" then curr else curr ^ ", ")
                  ^ "#" ^ s ^ "<>" ^ string_of_int i)
                "" lst
            ^ ")"
    end
  end

  type nonterm = int
  and term = int
  and sema = Tree.t -> Attr.t list -> Attr.t
  and 'a rema = Tree.t -> Attr.t -> 'a list -> 'a
  and predic = Tree.t -> Attr.t list -> bool

  and 'a core =
    | Normal of (nonterm * Tree.t * nonterm list * predic * sema * 'a rema)
    | Chain of (nonterm * nonterm * predic * sema * 'a rema)

  and 'a rule = 'a core * string

  let true' _ _ = true
  let rule (n, t, s, sema, sema') = Normal (n, t, s, true', sema, sema')
  let rule_p (n, t, s, pred, sema, sema') = Normal (n, t, s, pred, sema, sema')
  let chain (d, n, sema, sema') = Chain (d, n, true', sema, sema')
  let chain_p (d, n, pred, sema, sema') = Chain (d, n, pred, sema, sema')

  let toString = function
    | Normal (d, t, sons, _, _, _), lab ->
        Nonterm.toString d ^ ": " ^ Tree.toString t ^ " ("
        ^ List.fold_left
            (fun curr n ->
              (if curr = "" then curr else curr ^ ", ") ^ Nonterm.toString n)
            "" sons
        ^ ") {" ^ lab ^ "};"
    | Chain (d, n, _, _, _), lab ->
        Nonterm.toString d ^ ": " ^ Nonterm.toString n ^ "{" ^ lab ^ "};"

  module Table = struct
    open Array

    type 'a t = { normals : 'a row array; chains : 'a chain array }
    and 'a row = 'a rule list array
    and 'a chain = 'a rule list
    and 'a rc = Ok of 'a t | Fail of string

    exception Terminal of int * string
    exception Nonterm of int * string
    exception Derived of int * string
    exception Degree of int * string

    let build rules =
      try
        Ok
          ((fun rules (terminals, nonterminals, maxdegree) ->
             let table =
               {
                 normals = init terminals (fun _ -> make (maxdegree + 1) []);
                 chains = make nonterminals [];
               }
             in
             List.iter
               (fun rule ->
                 match rule with
                 | Normal (d, t, sons, _, _, _), lab ->
                     let t, s = (Tree.term t, List.length sons) in

                     if t < 0 || t >= terminals then raise (Terminal (t, lab));
                     if s > maxdegree then raise (Degree (s, lab));
                     if d < 0 || d >= nonterminals then raise (Derived (d, lab));

                     table.normals.(t).(s) <- rule :: table.normals.(t).(s)
                 | Chain (d, n, _, _, _), lab ->
                     if n < 0 || n >= nonterminals then raise (Nonterm (n, lab));
                     if d < 0 || d >= nonterminals then raise (Derived (d, lab));

                     table.chains.(n) <- rule :: table.chains.(n))
               rules;
             table)
             rules
             (Tree.terminals, Nonterm.number, Tree.degree))
      with
      | Terminal (n, lab) ->
          Fail
            ("wrong terminal #" ^ string_of_int n ^ " encountered in rule \""
           ^ lab ^ "\"")
      | Nonterm (n, lab) ->
          Fail
            ("wrong nonterminal #" ^ string_of_int n ^ " encountered in rule \""
           ^ lab ^ "\"")
      | Derived (n, lab) ->
          Fail
            ("wrong derived nonterminal #" ^ string_of_int n
           ^ " encountered in rule \"" ^ lab ^ "\"")
      | Degree (n, lab) ->
          Fail
            ("wrong degree " ^ string_of_int n ^ " encountered in rule \"" ^ lab
           ^ "\"")
      | x -> Fail ("exception \"" ^ Printexc.to_string x ^ "\" raised")

    let toString = function
      | Ok { normals = norm; chains = ch } ->
          let rules =
            List.fold_left
              (fun curr r -> curr ^ "\n" ^ "        " ^ toString r)
              ""
          in
          let by_degree a =
            fst
              (fold_left
                 (fun (curr, i) r ->
                   ( curr ^ "\n" ^ "      degree=" ^ string_of_int i ^ rules r,
                     i + 1 ))
                 ("", 0) a)
          in
          let by_nonterm a =
            fst
              (fold_left
                 (fun (curr, i) r ->
                   ( curr ^ "\n" ^ "    nonterm=" ^ Nonterm.toString i ^ rules r,
                     i + 1 ))
                 ("", 0) a)
          in
          "BeginTable\n" ^ "  normal rules:"
          ^ fst
              (fold_left
                 (fun (curr, i) r ->
                   ( curr ^ "\n" ^ "    terminal="
                     ^ Tree.toString (Tree.node i)
                     ^ by_degree r,
                     i + 1 ))
                 ("", 0) norm)
          ^ "\n\nchain rules:" ^ by_nonterm ch ^ "\n\nEndTable\n"
      | Fail reason -> "No table built: " ^ reason ^ ".\n"
  end

  module WTree = WrappedTree (Tree)
  module WNonterm = WrappedNonterm (Nonterm)

  module Labeler = struct
    exception Break
    exception Nomatch of Tree.t

    type 'a t = 'a labels * WTree.t
    and 'a elem = { attr : Attr.t; cost : int; rule : 'a rule }
    and 'a label = 'a elem option array
    and 'a labels = 'a label array
    and 'a rc = Ok of 'a t | Fail of string

    open Array

    let toString = function
      | Fail reason -> "cannot label: " ^ reason
      | Ok (table, tree) ->
          let rec inner ofs curr (WTree.Foo (_, inode, sons) as t) =
            List.fold_left
              (fun curr tree -> curr ^ inner (ofs + 2) "" tree)
              (String.make ofs ' ' ^ WTree.toString' t ^ ": "
              ^ fold_int 0 WNonterm.number "" (fun i curr ->
                    curr
                    ^
                    match table.(inode).(i) with
                    | Some { attr = a; cost = c; rule = _, lab } ->
                        WNonterm.toString i ^ " (attr=" ^ Attr.toString a
                        ^ "; cost=" ^ string_of_int c ^ "; rule=\"" ^ lab
                        ^ "\"); "
                    | None -> "")
              ^ "\n")
              sons
          in

          "Begin Covering\n" ^ inner 0 "" tree ^ "End Covering\n"

    let run' (root, nnodes) = function
      | Table.Fail reason -> Fail ("table not built: " ^ reason)
      | Table.Ok table -> (
          let labels =
            init nnodes (fun _ -> init WNonterm.number (fun _ -> None))
          in

          let rec process_node node =
            let term, degree =
              (WTree.term node, List.length (WTree.sons node))
            in
            let rules = table.Table.normals.(term).(degree) in

            let check_rule nodes nonterms =
              try
                let attrs, costs =
                  List.fold_left2
                    (fun (al, cl) node nonterm ->
                      match labels.(WTree.id node).(nonterm) with
                      | None -> raise Break
                      | Some { attr = a; cost = c; rule = _ } ->
                          (a :: al, c :: cl))
                    ([], []) nodes nonterms
                in
                Some (List.rev attrs, List.rev costs)
              with
              | Break -> None
              | x -> raise x
            in

            let rec apply_rule node derived pred sema attrs rule =
              let inode, tree = (WTree.id node, WTree.orig node) in
              let elem, attr = (labels.(inode).(derived), sema tree attrs) in
              let cost = Attr.cost attr in
              if pred tree attrs then
                match elem with
                | None ->
                    labels.(inode).(derived) <- Some { attr; cost; rule };
                    build_closure node derived
                | Some { attr = _; cost = c; rule = _ } when cost < c ->
                    labels.(inode).(derived) <- Some { attr; cost; rule };
                    build_closure node derived
                | _ -> ()
            and build_closure node nonterm =
              let t, i = (WTree.orig node, WTree.id node) in
              let chains = table.Table.chains.(nonterm) in
              List.iter
                (fun ((Chain (d, n, pred, sema, _), _) as rule) ->
                  let (Some { attr = a; cost = c; rule = _ }) =
                    labels.(i).(n)
                  in
                  apply_rule node d pred sema [ a ] rule)
                chains
            in

            let apply_rules rules node =
              List.iter
                (fun ((Normal (d, _, nonterms, pred, sema, _), _) as rule) ->
                  match check_rule (WTree.sons node) nonterms with
                  | Some (attrs, costs) ->
                      apply_rule node d pred sema attrs rule
                  | None -> ())
                rules;
              try
                iter
                  (fun e -> match e with Some _ -> raise Break | _ -> ())
                  labels.(WTree.id node);
                raise (Nomatch (WTree.orig node))
              with
              | Break -> ()
              | x -> raise x
            in

            List.iter (fun node -> process_node node) (WTree.sons node);

            apply_rules rules node
          in

          try
            process_node root;
            Ok (labels, root)
          with
          | Nomatch _ as nm -> raise nm
          | x -> Fail (Printexc.to_string x))

    let run tree table = run' (WTree.build tree) table
  end

  module Reducer = struct
    type 'a rc = Ok of 'a | Fail of string

    let toString = function Ok _ -> "ok" | Fail reason -> reason

    let rec reduce node nonterm labels =
      let i, t, sons = (WTree.id node, WTree.orig node, WTree.sons node) in
      let (Some { Labeler.attr; Labeler.cost = _; Labeler.rule }) =
        labels.(i).(nonterm)
      in

      match rule with
      | Normal (_, _, sons', _, _, sema), _ ->
          let sons, sons' =
            List.split
              (List.sort
                 (fun (node, nonterm) (node', nonterm') ->
                   let (Some s) = labels.(WTree.id node).(nonterm) in
                   let (Some s') = labels.(WTree.id node').(nonterm') in
                   Attr.( <= ) s.Labeler.attr s'.Labeler.attr)
                 (List.combine sons sons'))
          in
          sema t attr
            (List.map2
               (fun node' nonterm' -> reduce node' nonterm' labels)
               sons sons')
      | Chain (_, nonterm', _, _, sema), _ ->
          sema t attr [ reduce node nonterm' labels ]

    let run = function
      | Labeler.Fail reason -> Fail ("cannot reduce: " ^ reason)
      | Labeler.Ok (labels, tree) ->
          Ok
            (reduce tree
               (fst
                  (Array.fold_left
                     (fun (curr, (max, i)) lab ->
                       match lab with
                       | Some
                           {
                             Labeler.attr = _;
                             Labeler.cost = c;
                             Labeler.rule = _;
                           }
                         when c < max ->
                           (i, (c, i + 1))
                       | _ -> (curr, (max, i + 1)))
                     (0, (max_int, 0))
                     labels.(WTree.id tree)))
               labels)
  end

  module Verify = struct
    type t = Yes | No of string

    let run () =
      match (WTree.Verify.run (), WNonterm.Verify.run ()) with
      | WTree.Verify.Yes, WNonterm.Verify.Yes -> Yes
      | tree, nont ->
          No
            ("tree verification: " ^ WTree.Verify.toString tree ^ ", "
           ^ "nonterminals verification: "
            ^ WNonterm.Verify.toString nont)
  end

  let parse table tree = Reducer.run (Labeler.run tree table)

  let rec fold_left f t c =
    f (List.fold_left (fun c t -> fold_left f t c) c (Tree.sons t)) t

  let rec fold_right f t c =
    f (List.fold_right (fun t c -> fold_right f t c) (Tree.sons t) c) t
end

module BURS (Tree : Tree) (Nonterm : Nonterminal) = struct
  module Cost = struct
    type t = int

    let cost i = i
    let toString = string_of_int
    let ( <= ) = nosort
  end

  module Grammar = Grammar (Tree) (Nonterm) (Cost)

  type nonterm = Grammar.nonterm
  and term = Grammar.nonterm
  and cost = Tree.t -> int list -> int
  and 'a rema = Tree.t -> 'a list -> 'a
  and predic = Grammar.predic
  and 'a core = 'a Grammar.core
  and 'a rule = 'a core * string

  module Table = Grammar.Table
  module Verify = Grammar.Verify
  module Labeler = Grammar.Labeler
  module Reducer = Grammar.Reducer

  let toString = Grammar.toString
  let fold_left = Grammar.fold_left
  let fold_right = Grammar.fold_right
  let parse = Grammar.parse
  let rule (n, t, l, s, r) = Grammar.rule (n, t, l, s, fun x y z -> r x z)

  let rule_p (n, t, l, p, s, r) =
    Grammar.rule_p (n, t, l, p, s, fun x y z -> r x z)

  let chain (n, k, s, r) = Grammar.chain (n, k, s, fun x y z -> r x z)
  let chain_p (n, k, p, s, r) = Grammar.chain_p (n, k, p, s, fun x y z -> r x z)
end

module ConstBURS (Tree : Tree) (Nonterm : Nonterminal) = struct
  module Grammar = BURS (Tree) (Nonterm)
  module Table = Grammar.Table
  module Verify = Grammar.Verify
  module Labeler = Grammar.Labeler
  module Reducer = Grammar.Reducer

  type nonterm = Grammar.nonterm
  and term = Grammar.nonterm
  and cost = int
  and 'a rema = Tree.t -> 'a list -> 'a
  and 'a core = 'a Grammar.core
  and 'a rule = 'a core * string

  let rule (n, t, l, c, s) = Grammar.rule (n, t, l, (fun _ _ -> c), s)
  let chain (n, k, c, s) = Grammar.chain (n, k, (fun _ _ -> c), s)
  let toString = Grammar.toString
  let fold_left = Grammar.fold_left
  let fold_right = Grammar.fold_right
  let parse = Grammar.parse
end
