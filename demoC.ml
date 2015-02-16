open Batteries
open Escher_types
open SpecInfer

let lappend = fun (l0, l1) -> List.append l0 l1

let f = lappend

let tests = [
    ([],[]);
    ([],[1]);
    ([1;2],[1;2]);
    ([],[1;2;3;4]);
    ([1],[1;2;3]);
    ([1;2;3;4;5],[1;2]);
    ([1;2;3;4],[1;2;9;8]);
    ([1;2],[3;4]);
    ([1;2;3],[]);
    ([1],[1;1]);
    ([2],[])
]

let typo = []

let trans = (fun (x,y) -> [ of_list of_int x ; of_list of_int y ])

let def_features = [
    ((fun (l0,l1) -> ((fun l0 -> List.length(l0) > 0) l0)), "len(l0) > 0");
    ((fun (l0,l1) -> ((fun l0 -> List.length(l0) = 0) l0)), "len(l0) = 0");
    ((fun (l0,l1) -> ((fun l1 -> List.length(l1) > 0) l1)), "len(l1) > 0");
    ((fun (l0,l1) -> ((fun l1 -> List.length(l1) = 0) l1)), "len(l1) = 0");
    ((fun (l0,l1) -> ((fun l0 l1 -> l0 = l1) l0 l1)), "l0 = l1");
    ((fun (l0,l1) -> ((fun l0 l1 -> List.for_all (fun l0e -> (fun l0e l1 -> List.for_all (fun l1e -> (fun l0e l1e -> l0e = l1e) l0e l1e) l1) l0e l1) l0) l0 l1)), "∀ l0e in l0 -> ∀ l1e in l1 -> l0e = l1e");
    ((fun (l0,l1) -> ((fun l0 l1 -> List.for_all (fun l0e -> (fun l0e l1 -> List.exists (fun l1e -> (fun l0e l1e -> l0e = l1e) l0e l1e) l1) l0e l1) l0) l0 l1)), "∀ l0e in l0 -> ∃ l1e in l1 -> l0e = l1e");
    ((fun (l0,l1) -> ((fun l0 l1 -> List.exists (fun l0e -> (fun l0e l1 -> List.for_all (fun l1e -> (fun l0e l1e -> l0e = l1e) l0e l1e) l1) l0e l1) l0) l0 l1)), "∃ l0e in l0 -> ∀ l1e in l1 -> l0e = l1e");
    ((fun (l0,l1) -> ((fun l0 l1 -> List.exists (fun l0e -> (fun l0e l1 -> List.exists (fun l1e -> (fun l0e l1e -> l0e = l1e) l0e l1e) l1) l0e l1) l0) l0 l1)), "∃ l0e in l0 -> ∃ l1e in l1 -> l0e = l1e");
    ((fun (l0,l1) -> ((fun l0 l1 -> List.length(l0) > List.length(l1)) l0 l1)), "len(l0) > len(l1)");
    ((fun (l0,l1) -> ((fun l0 l1 -> List.length(l0) = List.length(l1)) l0 l1)), "len(l0) = len(l1)")
]

let my_features = []

let def_postconditions = [
    ((fun z r -> match r with Bad _ -> true | _ -> false), "exception thrown");
    ((fun z r -> match r with Ok _ -> true | _ -> false), "terminates normally");
    ((fun z r -> match r with Bad _ -> false | Ok res -> List.length(res) > 0), "len(res) > 0");
    ((fun z r -> match r with Bad _ -> false | Ok res -> List.length(res) = 0), "len(res) = 0");
    ((fun (l0,l1) r -> match r with Bad _ -> false | Ok res -> ((fun l0 res -> l0 = res) l0 res)), "l0 = res");
    ((fun (l0,l1) r -> match r with Bad _ -> false | Ok res -> ((fun l0 res -> List.length(l0) > List.length(res)) l0 res)), "len(l0) > len(res)");
    ((fun (l0,l1) r -> match r with Bad _ -> false | Ok res -> ((fun l0 res -> List.length(l0) = List.length(res)) l0 res)), "len(l0) = len(res)");
    ((fun (l0,l1) r -> match r with Bad _ -> false | Ok res -> ((fun l1 res -> l1 = res) l1 res)), "l1 = res");
    ((fun (l0,l1) r -> match r with Bad _ -> false | Ok res -> ((fun l1 res -> List.length(l1) > List.length(res)) l1 res)), "len(l1) > len(res)");
    ((fun (l0,l1) r -> match r with Bad _ -> false | Ok res -> ((fun l1 res -> List.length(l1) = List.length(res)) l1 res)), "len(l1) = len(res)")
]

let my_postconditions = [
    ((fun (l0,l1) r -> match r with Bad _ -> false | Ok lr -> List.length(lr) = List.length(l0) + List.length(l1)), "len(res) = len(l0)+len(l1)");
    ((fun (l0,l1) r -> match r with Bad _ -> false | Ok lr -> List.length(lr) != List.length(l0) + List.length(l1)), "len(res) != len(l0)+len(l1)")
]

let features = def_features @ my_features

let postconds = def_postconditions @ my_postconditions

let execef () = convergeAllFeatures f tests features postconds (typo, trans)

let execsi () = pacLearnSpec f tests features postconds (typo, trans)

let execMat () = printSignificantFeatures f tests features postconds (typo, trans)
