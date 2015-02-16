open Batteries
open Escher_types
open SpecInfer

let abs = fun x -> if x >= 3
                   then x
                   else if x >= (-7) && x <= (-2)
                        then raise Exit
                        else -x

let f = abs

let tests = [ 5 ; -2 ; 0 ; 10 ; -100 ; 32 ; -45 ; 64 ; -99 ; 128 ; -37 ; 1 ; 3 ; -4 ; 2 ]

let trans = fun x -> [ of_int x ]

let typo = []

let features = [
    ((fun i -> i > 0), "i > 0");
    ((fun i -> i = 0), "i = 0")
]

let postconds = [
    ((fun z r -> match r with Bad _ -> true | _ -> false), "exception thrown");
    ((fun z r -> match r with Ok _ -> true | _ -> false), "terminates normally");
    ((fun z r -> match r with Bad _ -> false | Ok res -> res > 0), "res > 0");
    ((fun z r -> match r with Bad _ -> false | Ok res -> res = 0), "res = 0");
    ((fun i r -> match r with Bad _ -> false | Ok res -> i > res), "i > res");
    ((fun i r -> match r with Bad _ -> false | Ok res -> i = res), "i = res")
]

let execef () = convergeAllFeatures f tests features postconds (typo, trans)

let execsi () = pacLearnSpec f tests features postconds (typo, trans)

let execMat () = printSignificantFeatures f tests features postconds (typo, trans)
