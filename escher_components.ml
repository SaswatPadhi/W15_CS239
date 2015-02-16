open Escher_core
open Escher_types

type component = {
      domain : typ list;
      codomain : typ;
      apply : value list -> value;
      name : string
    }

let select i list = List.map (fun x -> x.(i)) list

let apply_component (c : component) (args : Vector.t list) =
  let progs = List.map fst args in
  let values = List.map snd args in
  let result = Array.mapi (fun i _ -> c.apply (select i values)) (List.hd values)
  in
    let new_prog = fun ars -> c.apply (List.map (fun (p,_) -> p ars) progs)
  in
    ((new_prog, Node (c.name, List.map snd progs)), result)

(* Default INT components *)
let addone = {
    domain = [TInt];
    codomain = TInt;
    apply = (function
             | [VInt x] -> VInt (x + 1)
             | _      -> VError);
    name = "addone"
}

let subone = {
    domain = [TInt];
    codomain = TInt;
    apply = (function
             | [VInt x] -> VInt (x - 1)
             | _      -> VError);
    name = "subone"
}

let plus = {
    domain = [TInt; TInt];
    codomain = TInt;
    apply = (function
             | [VInt x; VInt y] -> VInt (x + y)
             | _ -> VError);
    name = "plus"
}

let minus = {
    domain = [TInt; TInt];
    codomain = TInt;
    apply = (function
             | [VInt x; VInt y] -> VInt (x - y)
             | _ -> VError);
    name = "minus"
}

let mult = {
    domain = [TInt; TInt];
    codomain = TInt;
    apply = (function
             | [VInt x; VInt y] -> VInt (x * y)
             | _ -> VError);
    name = "mult"
}

let leqzero = {
    domain = [TInt];
    codomain = TBool;
    apply = (function
             | [VInt x] -> VBool (x <= 0)
             | _ -> VError);
    name = "leq0"
}

let leq = {
    domain = [TInt;TInt];
    codomain = TBool;
    apply = (function
             | [VInt x; VInt y] -> VBool (x <= y)
             | _ -> VError);
    name = "leq"
}

let eq1 = {
    domain = [TInt];
    codomain = TBool;
    apply = (function
             | [VInt x] -> VBool (x = 1)
             | _ -> VError);
    name = "eq1"
}

let equal = {
    domain = [TInt;TInt];
    codomain = TBool;
    apply = (function
             | [VInt x;VInt y] -> VBool (x = y)
             | _ -> VError);
    name = "equal"
}

let modulo = {
    domain = [TInt;TInt];
    codomain = TBool;
    apply = (function
             | [VInt x;VInt y] -> if y = 0 then VError else VBool (x mod y = 0)
             | _ -> VError);
    name = "equal"
}


(* Default BOOL components *)
let notc = {
    domain = [TBool];
    codomain = TBool;
    apply = (function
             | [VBool x] -> VBool (not x)
             | _ -> VError);
    name = "not"
}


(* Default LIST components *)
let length = {
    domain = [TList];
    codomain = TInt;
    apply = (function
             | [VList xs] -> VInt (List.length xs)
             | _ -> VError);
    name = "length"
}

let empty = {
    domain = [TList];
    codomain = TBool;
    apply = (function
             | [VList x] -> VBool (List.length x = 0)
             | _ -> VError);
    name = "empty?"
}

let reverse = {
    domain = [TList];
    codomain = TList;
    apply = (function
             | [VList x] -> VList (List.rev x)
             | _ -> VError);
    name = "reverse"
}

let cons = {
    domain = [TInt; TList];
    codomain = TList;
    apply = (function
             | [VInt x; VList xs] -> VList (VInt x::xs)
             | _ -> VError);
    name = "cons"
}

let head = {
    domain = [TList];
    codomain = TInt;
    apply = (function
             | [VList (x::_)] -> x
             | _ -> VError);
    name = "head"
}

let tail = {
    domain = [TList];
    codomain = TList;
    apply = (function
             | [VList (_::xs)] -> VList xs
             | _ -> VError);
    name = "tail"
}

let cat = {
    domain = [TList; TList];
    codomain = TList;
    apply = (function [VList xs; VList ys] -> VList (xs @ ys)
             | _ -> VError);
    name = "cat"
}

let listEq = {
    domain = [TList;TList];
    codomain = TBool;
    apply = (function
             | [VList x; VList y] -> VBool (x=y)
             | _ -> VError);
    name = "listEq"
}

(* Default TREE components *)
let is_leaf = {
    domain = [TTree];
    codomain = TBool;
    apply = (function
             | [VTree (BTLeaf _)] -> VBool true
             | [VTree (BTNode (_,_,_))] -> VBool false
             | _ -> VError);
    name = "is_leaf"
}

let tree_val = {
    domain = [TTree];
    codomain = TInt;
    apply = (function
             | [VTree (BTLeaf x)] -> VInt x
             | [VTree (BTNode (x,_,_))] -> VInt x
             | _ -> VError);
    name = "tree_val"
}

let tree_left = {
    domain = [TTree];
    codomain = TTree;
    apply = (function
             | [VTree (BTNode (_,left,_))] -> VTree left
             | _ -> VError);
    name = "tree_left"
}

let tree_right = {
    domain = [TTree];
    codomain = TTree;
    apply = (function
             | [VTree (BTNode (_,_,right))] -> VTree right
             | _ -> VError);
    name = "tree_right"
}

let tree_node = {
    domain = [TInt; TTree; TTree];
    codomain = TTree;
    apply = (function
             | [VInt v; VTree l; VTree r] -> VTree (BTNode (v, l, r))
             | _ -> VError);
    name = "tree_node"
}

let tree_leaf = {
    domain = [TInt];
    codomain = TTree;
    apply = (function
             | [VInt n] -> VTree (BTLeaf n)
             | _ -> VError);
    name = "tree_leaf"
}

(* String components *)
let str_concat = {
    domain = [TString;TString];
    codomain = TString;
    apply = (function
             | [VString x; VString y] -> VString (x ^ y)
             | _ -> VError);
    name = "str_concat"
}

let str_len = {
    domain = [TString];
    codomain = TInt;
    apply = (function
             | [VString x] -> VInt (String.length x)
             | _ -> VError);
    name = "str_len"
}

let str_sub = {
    domain = [TString;TInt;TInt];
    codomain = TString;
    apply = (function
             | [VString str; VInt lo; VInt hi] ->
                   begin try VString (String.sub str lo hi)
                   with Invalid_argument _ -> VError end
             | _ -> VError);
    name = "str_sub"
}

let str_prefix = {
    domain = [TString;TInt];
    codomain = TString;
    apply = (function
             | [VString str; VInt hi] ->
                   begin try VString (String.sub str 0 hi)
                   with Invalid_argument _ -> VError end
             | _ -> VError);
    name = "str_prefix"
}

let str_suffix = {
    domain = [TString;TInt];
    codomain = TString;
    apply = (function
             | [VString str; VInt lo] ->
                   begin try VString (String.sub str lo ((String.length str) - lo))
                   with Invalid_argument _ -> VError end
             | _ -> VError);
    name = "str_suffix"
}

let rec palindrome_impl l =
  l = (List.rev l)

let palindrome = {
    domain = [TList];
    codomain = TBool;
    apply = (function
               [VList l] -> VBool (palindrome_impl l)
             | _ -> VError);
    name = "palindrome"
}

(* PACSpec Examples *)

let absPrecond = {
    domain = [TInt];
    codomain = TBool;
    apply = (function
             | [VInt x] -> if x = 1 then VBool false else if x = 0 then VBool true else if x = (-1) then VBool true else VError
             | _ -> VError);
    name = "absPrecond"
}
