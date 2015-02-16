open Escher_components
open Escher_core
open Escher_types

type task =
    { target : component;
      inputs : Vector.t list;
      components : component list }

let rec divide f arity target acc =
  if arity = 0 then
    if target = 0 then f acc else ()
  else begin
    for i = 1 to target do
      divide f (arity - 1) (target - i) (i::acc)
    done
  end

(* Rename to "divide" if using the "depth" heuristic, which violates our
    additivity assumption *)
let rec divide_depth f arity target acc =
  if arity = 0 then f acc
  else if arity = 1 && List.for_all (fun x -> x < target) acc
  then f (target::acc)
  else begin
    for i = 0 to target do
      divide f (arity - 1) target (i::acc)
    done
  end


(* Upper bound on the heuristic value a solution may take *)
let max_h = 8

let expand_ = ref "size"
let goal_graph = ref false

let noisy = ref false
let quiet = ref false

let all_solutions = ref []

let rec is_boring = function
  | (Leaf "0") | (Leaf "[]") -> true
  | (Leaf _) -> false
  | Node (_, args) -> List.for_all is_boring args

let rec fancy = function
  | Leaf _ -> 1
  | Node (x, args) -> 
      let h_values = List.map fancy args in
      let h_sum = List.fold_left (+) 0 h_values in
      let size = h_sum in (* estimate size with h_sum *)
      let penalty = 0 in
      let penalty =
	if size <= 3 then penalty
	else if is_boring (Node (x, args)) then 2 + penalty
	else penalty
      in
	1 + h_sum + penalty

let hvalue ((_,x),_) =
  match !expand_ with
    | "size" -> program_size x
    | "depth" -> program_height x
    | "fancy" -> fancy x
    | _ -> failwith "Unrecognized expand method!"

type goal_status =
  | Closed of Vector.t
  | Open

and goal =
    { varray : value array;
      mutable status : goal_status; }

let short_goal_string goal = match goal.status with
  | Open -> "<open> " ^ (varray_string goal.varray)
  | Closed v -> "<closed> " ^ (Vector.string v)

let rec print_goal indent goal =
  if String.length indent > 10 then print_endline (indent ^ "...")
  else print_endline (indent ^ "goal: " ^ (varray_string goal.varray))

let close_goal vector goal =
  if !noisy then begin
    print_endline ("Closed goal " ^ (varray_string goal.varray));
    print_endline ("       with " ^ (Vector.string vector));
  end;
  match goal.status with
    | Closed cls -> (* if (hvalue vector) < (hvalue cls) then *) goal.status <- Closed vector
    | Open -> goal.status <- Closed vector

let solve_impl task =
  let seen = ref VSet.empty in
  let vector_size = Array.length (snd (List.hd task.inputs)) in
  let components = task.components in

  let final_goal =
    { varray = snd (apply_component task.target task.inputs); status = Open; }
  in
  let goals =
    ref (VArrayMap.add final_goal.varray final_goal VArrayMap.empty)
  in

  let int_array = Array.make max_h VSet.empty in
  let bool_array = Array.make max_h VSet.empty in
  let list_array = Array.make max_h VSet.empty in
  let tree_array = Array.make max_h VSet.empty in
  let string_array = Array.make max_h VSet.empty in

  let check_vector v =
      (* Close all matching goals *)
      let (v_closes, _) = partition_map (varray_matches (snd v)) (!goals)
      in
        if !noisy then begin
          print_endline "--- new vector --------------------------------------";
          print_endline ((string_of_int (hvalue v)) ^ ": " ^ (Vector.string v));
        end;

	List.iter (close_goal v) v_closes; true
  in

  let int_components = List.filter (fun c -> c.codomain = TInt) components in
  let bool_components = List.filter (fun c -> c.codomain = TBool) components in
  let list_components = List.filter (fun c -> c.codomain = TList) components in
  let tree_components = List.filter (fun c -> c.codomain = TTree) components in
  let string_components = List.filter (fun c -> c.codomain = TString) components in

  let apply_comp f types i =
    let rec apply_cells types acc locations =
      match types, locations with
	| (typ::typs, i::locs) ->
	    VSet.iter (fun x -> apply_cells typs (x::acc) locs) begin
	      match typ with
		| TInt -> int_array.(i)
		| TBool -> bool_array.(i)
		| TList -> list_array.(i)
		| TTree -> tree_array.(i)
		| TString -> string_array.(i)
	    end
	| ([], []) -> f (List.rev acc)
	| _ -> failwith "Impossible!"
    in
      divide (apply_cells types []) (List.length types) (i-1) []
  in
  let expand_component c array i =
    let f x =
      let vector = apply_component c x in
      let h_value = hvalue vector in
      let has_err = Array.fold_left (fun p x -> match x with VError -> true | _ -> p) false (snd vector) in
(*	print_endline (string_of_int h_value ^ ">>" ^ (Vector.string vector));*)
	if (h_value < max_h && (not has_err))
	then array.(h_value) <- VSet.add vector array.(h_value)
    in
      apply_comp f c.domain i
  in
  let expand_type (mat, components) i =
    List.iter (fun c -> expand_component c mat i) components
  in
  let expand i =
    List.iter (fun x -> expand_type x i)
      [(int_array, int_components);
       (bool_array, bool_components);
       (list_array, list_components);
       (tree_array, tree_components);
       (string_array, string_components)]
  in
  let zero = (((fun ars -> VInt 0), Leaf "0"), Array.make vector_size (VInt 0)) in
  let nil = (((fun ars -> VList []), Leaf "[]"), Array.make vector_size (VList [])) in
  let btrue = (((fun ars -> VBool true), Leaf "true"), Array.make vector_size (VBool true)) in
  let bfalse = (((fun ars -> VBool false), Leaf "false"), Array.make vector_size (VBool false)) in
    print_endline ("Inputs: ");
    List.iter (fun v -> print_endline ("   " ^ (Vector.string v))) task.inputs;
    print_endline ("Goal: " ^ (varray_string final_goal.varray));
    list_array.(1) <- VSet.singleton nil;
    int_array.(1) <- VSet.singleton zero;
    bool_array.(1) <- VSet.add btrue (VSet.singleton bfalse);
    List.iter
      (fun input ->
	 let array = match (snd input).(1) with
	   | VList _ -> list_array
	   | VInt _ -> int_array
	   | VBool _ -> bool_array
	   | VTree _ -> tree_array
	   | VString _ -> string_array
	   | VError -> failwith "Error in input"
	   | VDontCare -> failwith "Underspecified input"
	 in
	   array.(1) <- VSet.add input array.(1))
      task.inputs;
    for i = 2 to max_h-1; do
      list_array.(i-1) <- VSet.filter check_vector list_array.(i-1);
      int_array.(i-1) <- VSet.filter check_vector int_array.(i-1);
      bool_array.(i-1) <- VSet.filter check_vector bool_array.(i-1);
      tree_array.(i-1) <- VSet.filter check_vector tree_array.(i-1);
      string_array.(i-1) <- VSet.filter check_vector string_array.(i-1);
      begin match final_goal.status with
    | Closed p -> all_solutions := p::all_solutions.contents ; final_goal.status <- Open
	| Open -> () end;
      if not (!quiet) then print_endline ("At " ^ (string_of_int i));
      if !noisy then begin
	let print_goal k _ = print_endline (" * " ^ (varray_string k)) in
	  print_endline ("Goals: ");
	  VArrayMap.iter print_goal (!goals);
      end;
      expand i;
    done

let solve task =
  all_solutions := [] ;
  solve_impl task ;
  print_endline "Synthesis Result: ";
  List.iter (fun v ->
      print_endline (Vector.string v))
    all_solutions.contents ;
  List.rev_map (fun s -> (program_string (snd (fst s)), (fun trans data -> (fst (fst s)) (trans data) = VBool true))) all_solutions.contents

let default_int = [plus;mult;minus;leq;equal;modulo ; leqzero;eq1;addone;subone]
let default_list = [empty;tail;head;cat;cons;length;reverse;listEq]
let default_bool = [notc]

let default_tree = [tree_val;is_leaf;tree_left;tree_right;tree_node;tree_leaf]
let default_string = [str_concat; str_len; str_sub; str_prefix; str_suffix]

let default_components =
  default_int @ default_list @ default_bool

let int_list xs = VList (List.map (fun x -> VInt x) xs)

let palindrome_task =
  {target = palindrome;
   inputs = [(((fun ars -> List.hd ars), Leaf "x"), [| VList [VBool true]; VList [VBool true; VBool false]; int_list []; int_list [6]; int_list [4;6]; int_list [6;4;6]; int_list [2;6;4;6]|])];
   components = listEq::reverse::default_components }

(* PACSpec Examples *)

let absPrecond_task =
  { target = absPrecond;
    inputs = [(((fun ars -> List.hd ars), Leaf "x"), [| VInt 0; VInt 1; VInt (-1)|])];
    components = default_components }

let test_task = {
    target = {
        domain = [TInt];
        codomain = TBool;
        apply = (fun [VInt x] -> VBool (x - 1 < 1));
        name = "test"
    };
    inputs = [(((fun ars -> List.hd ars), Leaf "x"), [| VInt 1; VInt 2; VInt (-1) |])];
    components = default_components
}

let synthesis_targets = [absPrecond_task; palindrome_task]

let synthesize target =
  let task =
    try List.find (fun x -> x.target.name = target) synthesis_targets
    with Not_found ->
      print_endline ("Task `" ^ target ^ "' not found.  Available tasks:");
      List.iter (fun task -> print_endline task.target.name) synthesis_targets;
      raise Not_found
  in
    solve task ; ()

let target =
  ("-target", Arg.String synthesize, " Synthesize a target program")

let expand_method =
  ("-expand", Arg.Set_string expand_, " Set expand method")

let noisy_arg =
  ("-noisy", Arg.Set noisy, " Print additional information")

let quiet_arg =
  ("-quiet", Arg.Set quiet, " Print less information")

let no_goal_graph_arg =
  ("-no-goal-graph", Arg.Clear goal_graph, " Disable goal graph")

let spec_list =
  [ target;
    expand_method;
    no_goal_graph_arg;
    noisy_arg;
    quiet_arg;
  ]
let usage_msg = "todo"
let anon_fun s = raise (Arg.Bad (s ^ " is not recognized"))

let _ = Arg.parse (Arg.align spec_list) anon_fun usage_msg
