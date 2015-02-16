(* we use the Batteries Included OCaml package *)

open Batteries
open Escher_core
open Escher_synth
open Escher_types

(* PAC learning a conjunction *)

(* a truth assignment is a mapping from boolean variables, which we represent by unique positive integers, to
   boolean values *)
type truthAssignment = (int, bool) BatHashtbl.t

(* thrown if there is no boolean function consistent with the given
   positive and negative examples.
   this happens, for example, if a positive and negative example have the
   same truthAssignment to variables, which is possible when learning specs,
   since our "features" may be insufficient to distinguish them.
*)
exception NoSuchFunction    

let string_of_truthAssignment ta =
  "[" ^ (BatHashtbl.fold (fun i b str -> str ^ "(" ^ (string_of_int i) ^ "," ^ (string_of_bool b) ^ "); ") ta "") ^ "]"

let hash_of_list l = BatHashtbl.of_enum(BatList.enum l)
    
    
(* remove literals from conj that are inconsistent with the given positive example *)
let pruneWithPositiveExamples (conj : int list) (example : truthAssignment) =
  BatList.filter (fun v -> BatHashtbl.find example v) conj

(* use a greedy heuristic to identify a set of literals in conj that cover all of the negative examples in
   remainingNeg (i.e., that conjunction of literals suffices to falsify all of the examples).
*)
let rec pruneWithNegativeExamples (conj : int list) (remainingNeg : truthAssignment list) : int list =
  match remainingNeg with
      [] -> []
    | _ ->
      (* for each variable in conj, count how many of the negative examples it covers
	 (i.e, on how many of the examples it has the truth value false) *)
      let counts =
	BatList.map
	  (fun var ->
	    (var, BatList.fold_left
	      (fun c ex ->
		if BatHashtbl.find_default ex var true then c else c+1) 0 remainingNeg)) conj in
      (* find the variable with the biggest count of covered negative literals.
	 in case of a tie we choose the variable whose id is smallest, since it's likely to represent
	 a smaller disjunction according to our encoding of CNF below.
      *)
      let (chosenVar, maxCount) =
	BatList.fold_left
	  (fun ((currChosen,currCount) as curr) ((v,c) as item) ->
	    if c > currCount then item else
	      if c = currCount && v < currChosen then item
	      else curr
	  ) (0,0) counts in

      (* if no literals cover any of the remaining negative examples, then
	 there is no boolean function that properly classifies all of the original
	 positive and negative examples *)
      if maxCount=0 then raise NoSuchFunction else
      
      (* keep the chosen variable and recurse,
	 filtering out this variable from the conjunction
	 and filtering out the negative examples that it covers.

	 we also filter out the negated version of the chosen
	 variable.  this is necessary when we are using this function
	 to find missing tests, so we don't say that (X and (not X))
	 is a missing test.  when this function is used as part of
	 learning a conjunction, there will be no negative variables
	 (see the comment on pacLearnConjunction about not including
	 negative literals), so it will be a no-op in that case.  it's
	 also not necessary in that case as long as we have already
	 pruned with at least one positive test case, which ensures
	 that a variable and its negation cannot both appear in conj *)
	
      chosenVar::(pruneWithNegativeExamples
		    (BatList.filter (fun v -> v <> chosenVar && v <> (-chosenVar)) conj)
		    (BatList.filter (fun ex -> BatHashtbl.find_default ex chosenVar true)
		     remainingNeg))
  
  

(* learn an unknown conjunct over the variables in list vars using the given set of
   positive and negative examples (list of truth assignments for which the unknown
   conjunct evaluates to true and false respectively).

   in the normal algorithm, you start with an initial conjunction of the form

     x1 and (not x1) and x2 and (not x2) and ... xn and (not xn)

   where the variables are x1...xn

   here we omit the negated ones because they are superfluous given our
   encoding of all possible disjuncts on the original variables as variables here
   (see the 3CNF learning algorithm below).

   so this is not a general algorithm for learning conjunctions

*)    
let pacLearnConjunction (vars : int list) (pos : truthAssignment list) (neg : truthAssignment list) =
  (* the initial conjunction is the AND of all variables *)
  let conj = vars in

  let conj = BatList.fold_left pruneWithPositiveExamples conj pos in
  
  pruneWithNegativeExamples conj neg

    
(* PAC learning a CNF formula *)


(* produce all pairs (considered as sets) of elements in l *)
let rec allPairs l =
  match l with
      [] -> []
    | x::xs -> (BatList.map (fun e -> (x,e)) xs)@(allPairs xs)

(* produce all triples (considered as sets) of elements in l *)
let rec allTriples l =
  let rec allTriplesOne l =
    match l with
	[] -> []
      | [_] -> []
      | x1::x2::xs -> (BatList.map (fun e -> (x1,x2,e)) xs)@(allTriplesOne (x1::xs)) in
  match l with
      [] -> []
    | x::xs -> (allTriplesOne l)@(allTriples xs) 
	

(* add a zero to the front of each pair in the given list *)	
let pad1 l =
  BatList.map (fun (x,y) -> (0,x,y)) l

(* add two zeros to the front of each element in the given list *)	
let pad2 l =
  BatList.map (fun x -> (0,0,x)) l

(* a type for cnf formulas, parameterized by the type used for atomic formulas *)    
type 'a literal = Pos of 'a | Neg of 'a
type 'a cnf = 'a literal list list

(* convert between representations for atomic formulas in a CNF formula *)
let mapCNF f clauses =
  BatList.map (fun clause ->
              BatList.map (fun lit ->
                         match lit with
			     Pos a -> Pos (f a)
			   | Neg a -> Neg (f a)) clause) clauses    

    
(* print a list, given a function to map each element to a string
   and a separator string to go between each element *)
let rec string_of_list_elems f sep l =
  BatString.concat sep (BatList.map f l)          

let string_of_clause_generic (string_of_a : 'a -> string) clause : string =
  let parenthesize s = "(" ^ s ^ ")" in
  let s =
    string_of_list_elems
      (fun lit ->
	 match lit with
	     Pos a -> string_of_a a
	   | Neg a -> "!(" ^ (string_of_a a) ^ ")")
      " || " clause in
    if (BatList.length clause) > 1 then parenthesize s else s 
    
let string_of_cnf_generic (string_of_a : 'a -> string) (cnf : 'a cnf) : string =
  match cnf with
      [] -> "true"
    | [[]] -> "false"
    | _ ->
	string_of_list_elems (string_of_clause_generic string_of_a) " && " cnf

let string_of_clause clause = string_of_clause_generic (fun x -> x) clause	  
let string_of_cnf cnf = string_of_cnf_generic (fun x -> x) cnf
	
exception TooManyVariables    

(* given n variables over a 3CNF formula to learn, we create
   one variable per possible 3-clause to use in the reduction to
   conjunction learning *)
let cnfVarsToConjunctVars n : (int * (int * int * int)) list =
    (* for our reduction to learning conjunctions we assume the total number of literals
       fits in 10 bits *)
  if (n > 500) then raise TooManyVariables else

  let poslits = if n=0 then [] else BatList.range 1 `To n in
  (* create a new set of variables for the reduction to learning a conjunction *)
  (* each represents a disjunct of up to three literals *)
  (* we represent a literal (not i) as a new variable i+n *)
  let size1 = pad2 (BatList.fold_left (fun l x -> x::x+n::l) [] poslits) in
  let size2 = pad1 (BatList.fold_left
		      (fun l (x,y) ->
			let nx = x+n in
			let ny = y+n in
			(x,y)::(x,ny)::(nx,y)::(nx,ny)::l) [] (allPairs poslits)) in
  let size3 =
    BatList.fold_left
      (fun l (x,y,z) ->
	let xneg = x+n in
	let yneg = y+n in
	let zneg = z+n in
	(x,y,z)::(x,y,zneg)::(x,yneg,z)::(x,yneg,zneg)::(xneg,y,z)::(xneg,y,zneg)::(xneg,yneg,z)::(xneg,yneg,zneg)::l)
      [] (allTriples poslits) in

  let newVars = size1@size2@size3 in
  
  (* pack three ints into one int uniquely by bit-shifting *)
  BatList.map (fun ((a,b,c) as t) -> (((a lsl 20) lor (b lsl 10) lor c), t)) newVars
  
(* PAC-learn a 3-CNF formula over the variables numbered 1 to n, given
   a set of positive and negative examples (list of truth assignments, each represented as a list of
   (int, bool) pairs. *)
let pacLearn3CNF (n : int) (pos : (int * bool) list list) (neg : (int * bool) list list) : int cnf =

  let varEncoding = cnfVarsToConjunctVars n in
  
  (* explicitly add truth values for negated literals to each example;
     also make variable 0 have the value false to uniformly handle encodings of singletons and pairs
  *)
  let augmentExamples examples =
    BatList.map
      (fun ex -> BatList.fold_left (fun curr ((i,b) as pos) -> pos::(i+n, not b)::curr) [(0,false)] ex) examples in

  let (pos, neg) = (augmentExamples pos, augmentExamples neg) in

  (* translate an example on the original variables to one on the new variables *)
  let encodeExample ex =
    let hex = hash_of_list ex in
    hash_of_list(
      BatList.map
	(fun (i, (x,y,z)) ->
	  (i, (BatHashtbl.find hex x) || (BatHashtbl.find hex y) || (BatHashtbl.find hex z)))
	varEncoding) in
    
  let newPos = BatList.map encodeExample pos in
  let newNeg = BatList.map encodeExample neg in

  (* learn a conjunction on the new variables *)
  let learnedConjunct = pacLearnConjunction (BatList.map (fun (i,_) -> i) varEncoding) newPos newNeg in

  (* translate the result back to the old variables *)
  let learned3CNF = BatList.map (fun i -> (i lsr 20, (i lsr 10) land 0x3ff, i land 0x3ff)) learnedConjunct in

  (* convert the result into a slightly more palatable form *)
  let indexToLit i = if i <= n then Pos i else Neg (i-n) in
  let disjunctToClause tuple =
    match tuple with
	(0,0,z) -> [indexToLit z]
      | (0,y,z) -> [indexToLit y; indexToLit z]
      | (x,y,z) -> [indexToLit x; indexToLit y; indexToLit z] in

  BatList.map disjunctToClause learned3CNF


    
(* PAC Learning Specifications *)

 (* cnfopt has type string cnf option
  * post has type string
  * *)
let print_spec (cnfopt, post) =
  match cnfopt with
      None -> print_string ("features are insufficient to separate positive and negative examples for postcondition " ^ post ^ "\n")
    | Some cnf ->
      print_string ("precondition: " ^ (string_of_cnf cnf) ^ "\n");
      print_string ("postcondition: " ^ post ^ "\n")

let print_specs specs =
  BatList.iter (fun s -> (print_spec s); print_newline()) specs

(* the result of evaluating the function whose spec we are learning *)    
type 'a result = ('a, exn) BatResult.t

(* tests is a set of inputs on which to test a particular function
   features is a set of predicates on inputs that we use as features for our learning
   incompatible is a set of partial truth assignments for features that are never satisfiable.
   for that we refer to a feature by its number in the features list, and we refer to its negation
   by the negative of that number.
   the result is a (small) logical conjunction which none of the given tests satisfies.
*)
let missingTests ~tests:(tests : 'a list) ~features:(features : (('a -> bool) * string) list) ?(incompatible = []) ()
    : string literal list option =

  if features = [] then None else
  
  (* create the truth assignment corresponding to each test in tests by evaluating the features *)
  let examples =
    BatList.unique
      (BatList.map
	 (fun arg ->
	    (* if a feature throws an exception treat it as if the feature returns false *)
	    BatList.mapi (fun index (p,_) -> (index + 1, try p arg with _ -> false)) features) tests) in

  (* treat the incompatible assignments as if they are examples that we have already covered *)
  let examples = examples@incompatible in
    
  (* explicitly add to each truth assignment a mapping for negative literals.
     we treat -n as the negative version of variable n
  *)
  let examples =
    BatList.map
      (fun ex ->
	hash_of_list(
	  BatList.flatten (BatList.map (fun (n,b) -> [(n,b);(-n,not b)]) ex))) examples in

  let len = BatList.length features in

  let allvars = (BatList.range (-len) `To (-1))@(BatList.range 1 `To len) in

    try
      let missing = pruneWithNegativeExamples allvars examples in
	
	Some (BatList.map (fun i ->
			     let (_,s) = BatList.nth features ((BatInt.abs i)-1) in
			       if i>0 then Pos s else Neg s) missing)
    with
	NoSuchFunction -> None
	  

(* this function takes the same arguments as does pacLearnSpec except
   it takes only one postcondition rather than a list of them.  the
   function returns groups of tests that illustrate a missing
   feature. each group has the property that all inputs in the group
   lead to the same truth assignment of features, but the group
   contains at least one positive and one negative example (in terms
   of the truth value of the given postcondition).  hence the user
   needs to provide a new feature that can properly separate these
   positive and negative examples.
*)
   
let missingFeatures (f : 'a -> 'b) ~tests:(tests : 'a list) ~features:(features : (('a -> bool) * string) list)
    ~postcondition:((postcond, str) : ('a -> 'b result -> bool) * string) =
  (* map each test input to its vector of feature booleans *)
  let examples =
    BatList.map
      (fun arg ->
	 (* if a feature throws an exception treat it as if the feature returns false *)
	 (arg, (BatList.mapi (fun index (p,_) -> (index + 1, try p arg with _ -> false)) features))) tests in

  let grouped = BatList.group (fun (arg1,ex1) (arg2,ex2) -> compare ex1 ex2) examples in

  (* filter out groups of size 1 *)
  let grouped = BatList.filter (fun l -> (BatList.length l) > 1) grouped in

  (* compute the result value and associated postcondition truth value for each argument in a group *)
  let addPost = BatList.map (fun group ->
    BatList.map (fun (arg, ex) -> let res = BatResult.catch f arg in
				  try
				    let post = postcond arg res in
				    (arg, res, ex, post)
				  with
				      _ ->
					(arg, res, ex, false)) group) grouped in

  (* only keep the groups that have a conflict on the postcondition's truth value *)
  let filtered =
    BatList.filter
      (fun group ->
	(BatList.exists (fun (_,_,_,b) -> b) group) &&
	(BatList.exists (fun (_,_,_,b) -> not b) group)) addPost in

  filtered

let synthFeatures (f : 'a -> 'b) ~tests:(tests : 'a list) ~missing_features:(missing_features : ('a * ('b, exn) BatResult.t * (int * bool) list * bool) list)
    ~postcondition:(postcond: ('a -> 'b result -> bool) * string) ~trans:(trans: typ list * ('a -> value list)) : (('a -> bool) * string) list =

    (* TODO-maybe:
        - Only take a pair of conflicts not whole group
        - Ensure that the synthed features now satisfy the whole group
    *)

    if missing_features = [] then []
    else (
        let tab = BatHashtbl.create (List.length missing_features) in
            BatList.iter (fun (i, _, _, b) -> BatHashtbl.add tab ((snd trans) i) (VBool b)) missing_features;
            let xtask = {
                target = {
                    domain = (fst trans);
                    codomain = TBool;
                    apply = (fun t -> BatHashtbl.find tab t);
                    name = "blah"
                };
                inputs = BatList.mapi (fun i _ -> (((fun ars -> List.nth ars i), Leaf ("x" ^ (string_of_int i))),
                                                   Array.of_list (BatHashtbl.fold (fun k _ acc -> (List.nth k i)::acc) tab []))) (fst trans);
                components = default_components
            } in
            List.map (fun (annot, func) -> (fun data -> func (snd trans) data), "gen: " ^ annot) (solve xtask))


let rec convergeOneMissingFeature (f: 'a -> 'b) ~tests:(tests: 'a list) ~missing_features:(missing_features: ('a * ('b, exn) BatResult.t * (int * bool) list * bool) list list)
    ~postcondition:(postcond: ('a -> 'b result -> bool) * string) ~trans:(trans: typ list * ('a -> value list)) : (('a -> bool) * string) list =

    if missing_features = [] then []
    else let new_features = synthFeatures f tests (List.hd missing_features) postcond trans
         in if new_features = [] then convergeOneMissingFeature f tests (List.tl missing_features) postcond trans
            else new_features


let rec convergePCondFeatures (f: 'a -> 'b) ~tests:(tests : 'a list) ~features:(features: (('a -> bool) * string) list)
    ~pcond:(postcond: ('a -> 'b result -> bool) * string) ~trans:(trans: typ list * ('a -> value list)) :(('a -> bool) * string) list =

    let all_missing_features = missingFeatures f tests features postcond in
        if all_missing_features = []
        then features
        else let mf = convergeOneMissingFeature f tests all_missing_features postcond trans
             in if mf = [] then features else convergePCondFeatures f tests (features @ mf) postcond trans


let rec convergeAllFeatures (f: 'a -> 'b) ~tests:(tests : 'a list) ~features:(features: (('a -> bool) * string) list)
    ~pconds:(postconds: (('a -> 'b result -> bool) * string) list) ~trans:(trans: typ list * ('a -> value list)) :(('a -> bool) * string) list =

    if postconds = [] then features else convergeAllFeatures f tests (convergePCondFeatures f tests features (List.hd postconds) trans) (List.tl postconds) trans


(* f is the function whose spec we are inferring
   tests is a set of inputs on which to test f
   features is a set of predicates on inputs that we use as features for our learning
   posts is the set of postconditions whose corresponding preconditions formula we are trying to learn
   we associate a human-readable string representation with each feature and postcondition
*)
let pacLearnSpec (f : 'a -> 'b) ~tests:(tests : 'a list) ~features:(features : (('a -> bool) * string) list)
    ~postconditions:(posts : (('a -> 'b result -> bool) * string) list) ~trans:(trans : typ list * ('a -> value list))
    : (string cnf option * string) list =

  let features = if fst trans = [] then features else convergeAllFeatures f tests features posts trans in

  let featureLen = BatList.length features in
  
  (* create the truth assignment corresponding to each test in tests by evaluating the features *)
  let examples =
    BatList.map
      (fun arg ->
	 (* if a feature throws an exception treat it as if the feature returns false *)
	 BatList.mapi (fun index (p,_) -> (index + 1, try p arg with _ -> false)) features) tests in
  
  (* run all the tests to get their outputs *)
  let testResults = BatList.map (fun test -> (test, BatResult.catch f test)) tests in


  let pacLearnOne (postcond, str) =

  (* separate the tests into positive and negative examples *)
    let (pos, neg) =
      BatList.fold_left2 (fun (l1,l2) (arg, res) ex -> try if postcond arg res then (ex::l1,l2) else (l1,ex::l2) with _ -> (l1,ex::l2))
        ([],[]) testResults examples in
      
    (* remove duplicates *)
    let (pos, neg) = (BatList.unique pos, BatList.unique neg) in

      (* if there are no positive examples then we return a representation of the precondition for False (an empty disjunct) *)
    if (pos = []) then (Some [[]], str) else
      try 
	let cnf = pacLearn3CNF featureLen pos neg in
	let precond = mapCNF (fun i -> let (_,s) = BatList.nth features (i-1) in s) cnf in
	(Some precond, str)
      with
	  NoSuchFunction -> (None, str) in

  BatList.map pacLearnOne posts


let learnAndPrintSpec (f : 'a -> 'b) (tests : 'a list) (features : (('a -> bool) * string) list)
    (posts : (('a -> 'b result -> bool) * string) list) (trans : typ list * ('a -> value list)) : unit =
  print_specs (pacLearnSpec f tests features posts trans)
