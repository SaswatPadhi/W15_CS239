open Batteries
open Escher_types
open Printf
open SpecInfer


let genDataMatrix (f: 'a -> 'b) ~tests:(tests: 'a list) ~features:(features: (('a -> bool) * string) list)
    ~postconditions:(posts: (('a -> 'b result -> bool) * string) list) ~trans:(trans: typ list * ('a -> value list))
    : int list list * int list list =

  let features = if fst trans = [] then features else convergeAllFeatures f tests features posts trans in

  let featureLen = BatList.length features in

  let f_true_coeff = 1 and f_false_coeff = -1 in
  let p_true_coeff = 1 and p_false_coeff = 0 in
  
  (* create the truth assignment corresponding to each test in tests by evaluating the features *)
  let featureVecs =
    BatList.map
      (fun arg ->
	  (* if a feature throws an exception treat it as if the feature returns false *)
      BatList.map (fun (p,_) -> try if p arg then f_true_coeff else f_false_coeff with _ -> f_false_coeff) features) tests in
  
  (* run all the tests to get their outputs *)
  let testResults = BatList.map (fun test -> (test, BatResult.catch f test)) tests in

  (* results for all the postconditions *)
  let postResults = BatList.map (fun (args,res) -> BatList.map (fun (p,_) -> try if p args res then p_true_coeff else p_false_coeff with _ -> p_false_coeff) posts) testResults in

  (featureVecs, postResults)


let acompare (a,_) (b,_) = - (compare (abs_float(a)) (abs_float(b)))


let doLogisticRegInR (f: 'a -> 'b) ~tests:(tests: 'a list) ~features:(features: (('a -> bool) * string) list)
    ~postconditions:(posts: (('a -> 'b result -> bool) * string) list) ~trans:(trans: typ list * ('a -> value list))
    : (float * (('a -> bool) * string)) list list =

    let dfile = "temp.arff" and ofile = "temp.res" and pfile = "temp.R" in
    let (fs, ps) = genDataMatrix f tests features posts trans in
        (let oc = open_out dfile in
            fprintf oc "%% Program data exported from specInfer (OCaml) project\n";
            fprintf oc "\n%% Features:\n";
            BatList.mapi (fun i _ -> fprintf oc "@attribute f%d numeric\n" i) features;

            fprintf oc "\n%% Post-Conditions:\n";
            BatList.mapi (fun i _ -> fprintf oc "@attribute p%d numeric\n" i) posts;

            fprintf oc "\n@data\n";
            fprintf oc "%s" (String.concat "\n" (BatList.map2 (fun f p -> (String.concat "," (BatList.map string_of_int f)) ^ "," ^
                                                                     (String.concat "," (BatList.map string_of_int p))) fs ps));
            close_out oc);
        (let oc = open_out pfile in
            fprintf oc "# R program generated by specInfer to find a regression model for postconditions\n\n";
            fprintf oc "library(foreign) ; mdata <- read.arff('%s')\n\n" dfile;
            let allfs = String.concat "+" (BatList.mapi (fun i _ -> "f" ^ string_of_int i) (List.hd fs)) in (
                BatList.iteri (fun i _ -> fprintf oc "p%dcoef <- coef(glm(p%d ~ %s, data=mdata, family=binomial()))\n" i i allfs) posts);
            fprintf oc "\npcoef <- rbind(%s)[,-1]\n" (String.concat "," (BatList.mapi (fun i _ -> "p" ^ (string_of_int i) ^ "coef") posts));
            fprintf oc "pcoef[is.na(pcoef)] <- 0\n";
            fprintf oc "write.table(pcoef, file='%s', sep=',', row.names=FALSE, col.names=FALSE)" ofile;
            close_out oc);
    Sys.command ("R <" ^ pfile ^ " --slave --no-save");
    let ic = open_in ofile in
        let res = List.map (fun _ -> BatList.(fast_sort acompare (map2 (fun f s -> (float_of_string s, f)) features (Str.split (Str.regexp ",") (input_line ic))))) posts in
        close_in ic ; res


let printSignificantFeatures (f: 'a -> 'b) ~tests:(tests: 'a list) ~features:(features: (('a -> bool) * string) list)
    ~postconditions:(posts: (('a -> 'b result -> bool) * string) list) ~trans:(trans: typ list * ('a -> value list))
    : unit  =

    let signi = doLogisticRegInR f tests features posts trans in
        BatList.iter2 (fun (_, ps) vs -> printf "\n\n%s =>" ps;
                                         List.iter (fun (v,(_,s)) -> if abs_float v > 0.693 then printf "\n    %48s [%5.1f]" s v) vs;
                                         print_newline())
                      posts signi