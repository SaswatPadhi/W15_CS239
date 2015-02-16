type bin_tree =
  | BTLeaf of int
  | BTNode of int * bin_tree * bin_tree

type typ =
  | TInt
  | TBool
  | TList
  | TTree
  | TString

type value =
  | VInt of int
  | VBool of bool
  | VList of value list
  | VTree of bin_tree
  | VString of string
  | VError
  | VDontCare

let of_int i = VInt(i)
let of_bool b = VBool(b)
let of_string s = VString(s)
let of_tree btree = VTree(btree)
let of_list f l = VList(List.map f l)

let wrap_escher_type transformer data =
  transformer data
