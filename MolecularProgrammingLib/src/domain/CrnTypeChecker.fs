(*
    Date: XX.06.2023
    Author: 
*)
module CrnTypeChecker

open CrnTypes

let findCmp cmds =
    List.exists
        (function
        | Comp(Mod(Cmp _)) -> true
        | _ -> false)
        cmds

let findConditional cmds =
    List.exists
        (function
        | Cond _ -> true
        | _ -> false)
        cmds

(* A cmp statement is required in a prior step in order to check any conditionals *)
let rec cmpBeforeConditionals =
    function
    | [] -> true
    | Step(cmds) :: rest ->
        if findConditional cmds then false
        elif findCmp cmds then true
        else cmpBeforeConditionals rest
    | _ :: rest -> cmpBeforeConditionals rest

(* Either, a species’s value is used in computations in one step or it is over-
written by one computation *)

let getModuleSources =
    function
    | Ld(a, b) -> [ a ]
    | Add(a, b, c) -> [ a; b ]
    | Sub(a, b, c) -> [ a; b ]
    | Mul(a, b, c) -> [ a; b ]
    | Div(a, b, c) -> [ a; b ]
    | Sqrt(a, b) -> [ a ]
    | Cmp(a, b) -> [ a; b ]

let getComputationSources =
    function
    | Mod(m) -> Set(getModuleSources m)
    | Rxn(r, p, n) -> Set r

let getConditionalComputations =
    function
    | IfGT(cmds) -> cmds
    | IfGE(cmds) -> cmds
    | IfEQ(cmds) -> cmds
    | IfLT(cmds) -> cmds
    | IfLE(cmds) -> cmds

let getCommandSources =
    function
    | Comp(comp) -> getComputationSources comp
    | Cond(cond) ->
        cond
        |> getConditionalComputations
        |> List.map getComputationSources
        |> Set.unionMany

let rec getLoads =
    function
    | Comp(Mod(Ld(a, b))) -> Set [ b ]
    | Cond(cond) ->
        cond
        |> getConditionalComputations
        |> List.map Comp
        |> List.map getLoads
        |> Set.unionMany
    | _ -> Set.empty

let getCmdsSourcesAndLoads cmds =
    (cmds |> List.map getCommandSources |> Set.unionMany, cmds |> List.map getLoads |> Set.unionMany)

let rec noLoadUseProp =
    function
    | [] -> true
    | Conc(_, _) :: rest -> noLoadUseProp rest
    | Step(cmds) :: rest ->
        let (sources, loads) = getCmdsSourcesAndLoads cmds
        let overlap = Set.intersect sources loads
        printfn "%A\n%A\n%A" sources loads overlap
        (Set.isEmpty overlap) && (noLoadUseProp rest)

let checkModuleArgs =
    function
    | Ld(A, B) -> A <> B
    | Add(A, B, C) -> C <> A && C <> B
    | Sub(A, B, C) -> C <> A && C <> B
    | Mul(A, B, C) -> C <> A && C <> B
    | Div(A, B, C) -> C <> A && C <> B
    | Sqrt(A, B) -> A <> B
    | Cmp(A, B) -> A <> B

let checkReactionArgs (r, p, n) =
    not (List.isEmpty r && List.isEmpty p) && n > 0.0

let rec checkCommandArgs =
    function
    | Comp(Mod(m)) -> checkModuleArgs m
    | Comp(Rxn(rxn)) -> checkReactionArgs rxn
    | _ -> true

let rec validArgsProp =
    function
    | [] -> true
    | Conc(_, c) :: rest -> c >= 0.0 && validArgsProp rest
    | Step(cmds) :: rest -> List.forall checkCommandArgs cmds && validArgsProp rest

let isWellFormedCrn (CRN prog) =
    cmpBeforeConditionals prog && noLoadUseProp prog && validArgsProp prog
