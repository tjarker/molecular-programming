(*
    Date: XX.06.2023
    Author: 
*)
module CrnTypeChecker

open CrnTypes


(*

    TODO: make load-use prop consider exclusivity of branches
*)
let exclusivConditions cond =
    function
    | (IfGT _) ->
        match cond with
        | (IfLT _)
        | (IfLE _)
        | (IfEQ _) -> true
        | _ -> false
    | (IfGE _) ->
        match cond with
        | (IfLT _) -> true
        | _ -> false
    | (IfEQ _) ->
        match cond with
        | (IfGT _)
        | (IfLT _) -> true
        | _ -> false
    | (IfLE _) ->
        match cond with
        | (IfGT _) -> true
        | _ -> false
    | (IfLT _) ->
        match cond with
        | (IfGT _)
        | (IfGE _)
        | (IfEQ _) -> true
        | _ -> false

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
    | Ld(a, _) -> [ a ]
    | Add(a, b, _) -> [ a; b ]
    | Sub(a, b, _) -> [ a; b ]
    | Mul(a, b, _) -> [ a; b ]
    | Div(a, b, _) -> [ a; b ]
    | Sqrt(a, _) -> [ a ]
    | Cmp(a, b) -> [ a; b ]

let getComputationSources =
    function
    | Mod(m) -> (getModuleSources m)
    | Rxn(r, p, n) -> r

let getConditionalComputations =
    function
    | IfGT(cmds) -> cmds
    | IfGE(cmds) -> cmds
    | IfEQ(cmds) -> cmds
    | IfLT(cmds) -> cmds
    | IfLE(cmds) -> cmds

let getCommandSources =
    function
    | Comp(comp) -> getComputationSources comp |> List.map (fun sp -> (None, sp))
    | Cond(cond) ->
        cond
        |> getConditionalComputations
        |> List.collect getComputationSources
        |> List.map (fun sp -> (Some cond, sp))

let rec getLoads =
    function
    | Comp(Mod(Ld(a, b))) -> [ None, b ]
    | Cond(cond) ->
        cond
        |> getConditionalComputations
        |> List.map Comp
        |> List.collect getLoads
        |> List.map (fun (_, sp) -> (Some cond, sp))
    | _ -> []

let getCmdsSourcesAndLoads cmds =
    (cmds |> List.collect getCommandSources, cmds |> List.collect getLoads)


let noLoadUseCollision (con0, src) (con1, ld) =
    match (src = ld, con0, con1) with
    | (false, _, _) -> true
    | (true, Some c0, Some c1) when exclusivConditions c0 c1 -> true
    | _ -> false

let rootNoLoadUseProp =
    function
    | Conc(_, _) -> true
    | Step(cmds) ->
        let (sources, loads) = getCmdsSourcesAndLoads cmds
        List.forall (fun src -> List.forall (noLoadUseCollision src) loads) sources

let noLoadUseProp roots = roots |> List.forall rootNoLoadUseProp

let checkModuleArgs strict =
    function
    | Ld(A, B) -> A <> B
    | Add(A, B, C) -> C <> A && C <> B
    | Sub(A, B, C) -> (C <> A && C <> B) || (strict && A <> B)
    | Mul(A, B, C) -> (C <> A && C <> B) || (strict && A <> B)
    | Div(A, B, C) -> C <> A && C <> B
    | Sqrt(A, B) -> A <> B
    | Cmp(A, B) -> A <> B

let checkReactionArgs (r, p, n) =
    not (List.isEmpty r && List.isEmpty p) && n > 0.0

let checkComputationArgs strict =
    function
    | Mod m -> checkModuleArgs strict m
    | Rxn rxn -> checkReactionArgs rxn

let rec checkCommandArgs strict =
    function
    | Comp comp -> checkComputationArgs strict comp
    | Cond cond -> getConditionalComputations cond |> List.forall (checkComputationArgs strict)

let rec validArgsProp strict =
    function
    | [] -> true
    | Conc(_, c) :: rest -> c >= 0.0 && validArgsProp strict rest
    | Step(cmds) :: rest -> List.forall (checkCommandArgs strict) cmds && validArgsProp strict rest



let moduleGetAssignment =
    function
    | Add(a, b, c) -> [ None, c ]
    | Sub(a, b, c) -> [ None, c ]
    | Mul(a, b, c) -> [ None, c ]
    | Div(a, b, c) -> [ None, c ]
    | Sqrt(a, b) -> [ None, b ]
    | Ld(a, b) -> [ None, b ]
    | Cmp(a, b) -> []

let rec count y acc =
    function
    | [] -> acc
    | x :: xs -> count y (if y = x then acc + 1 else acc) xs

let netChange sp ((r, p, _): Reaction) =
    let leftCount = count sp 0 r
    let rightCount = count sp 0 p
    rightCount - leftCount

let computationAssignments =
    function
    | Mod m -> moduleGetAssignment m
    | Rxn(r, p, n) ->
        let species = List.distinct (r @ p)
        let nonCats = List.filter (fun sp -> netChange sp (r, p, n) <> 0) species
        nonCats |> List.map (fun sp -> None, sp)

let conditionalAssignments cond =
    cond
    |> getConditionalComputations
    |> List.collect computationAssignments
    |> List.map (fun (_, sp) -> Some(cond), sp)

let rec forAllUniquePairs pred acc =
    function
    | []
    | [ _ ] -> acc
    | x :: xs ->
        let res = List.forall (pred x) xs
        forAllUniquePairs pred (acc && res) xs

let commandAssignments =
    function
    | Comp c -> computationAssignments c
    | Cond c -> conditionalAssignments c

let stepAssignments =
    function
    | Conc(_, _) -> []
    | Step cmds -> List.map commandAssignments cmds |> List.concat

let noAssignmentCollision (c0, sp0) (c1, sp1) =
    match (sp0 = sp1, c0, c1) with
    | (false, _, _) -> true
    | (true, Some(con0), Some(con1)) when exclusivConditions con0 con1 -> true
    | _ -> false

let singleAssignmentsPerStep (root) =
    root |> stepAssignments |> forAllUniquePairs noAssignmentCollision true

let singleAssignmentForAllSteps roots =
    roots |> List.forall singleAssignmentsPerStep

let computationCmpCount =
    function
    | Mod(Cmp(_, _)) -> None, true
    | _ -> None, false

let commandCmpCount =
    function
    | Comp c -> [ computationCmpCount c ]
    | Cond c ->
        getConditionalComputations c
        |> List.map computationCmpCount
        |> List.map (fun (_, cnt) -> Some c, cnt)

let stepCmpCount =
    function
    | Conc(_, _) -> []
    | Step(cmds) -> cmds |> List.collect commandCmpCount

let noCmpCollision (c0, isBr0) (c1, isBr1) =
    match (isBr0, isBr1, c0, c1) with
    | (false, _, _, _) -> true
    | (_, false, _, _) -> true
    | (true, true, Some(con0), Some(con1)) when exclusivConditions con0 con1 -> true
    | _ -> false

let singleCmpPerStep root =
    root |> stepCmpCount |> forAllUniquePairs noCmpCollision true

let singleCmpForAllSteps roots = roots |> List.forall singleCmpPerStep


let moduleSrcDest = function 
    | Add(a,b,c) -> [a;b], Some c
    | Sub(a,b,c) -> [a;b], Some c
    | Mul(a,b,c) -> [a;b], Some c
    | Div(a,b,c) -> [a;b], Some c
    | Ld(a,b) -> [a], Some b
    | Sqrt(a,b) -> [a], Some b
    | Cmp(a,b) -> [a;b], None

let computationSrcDest = function 
    | Mod m -> moduleSrcDest m
    | _ -> failwith "Not supported for reactions"

let conditionalSrcDests cond = 
    Some cond, getConditionalComputations cond
        |> List.map computationSrcDest

let commandSrcDests = function 
    | Comp c -> None, [computationSrcDest c]
    | Cond c -> conditionalSrcDests c

let noLaterProducer (asrc,_) = 
    List.forall (fun (_,bd) ->
        match bd with
        | Some d -> not (List.contains d asrc)
        | None -> true
    )

let rec noLaterProducerForAll = 
    function 
    | [] -> true
    | x::xs -> noLaterProducer x xs && (noLaterProducerForAll xs)

let branchIsActive eq gt = 
    function
    | Some(IfGT _) -> not eq && gt
    | Some(IfGE _) -> eq || gt
    | Some(IfEQ _) -> eq
    | Some(IfLE _) -> eq || (not gt)
    | Some(IfLT _) -> (not eq) && (not gt)
    | None -> true

let chooseBranchesByFlags eq gt xs = 
    List.collect (fun (condOpt, comps) ->
            if branchIsActive eq gt condOpt then comps else []
        ) xs

let dependencyOrderPropStep = 
    function 
    | Conc(_,_) -> true
    | Step cmds ->
        let srcsDests = List.map commandSrcDests cmds
        let gt = chooseBranchesByFlags false true srcsDests |> noLaterProducerForAll
        let eq = chooseBranchesByFlags true false srcsDests |> noLaterProducerForAll
        let lt = chooseBranchesByFlags false false srcsDests |> noLaterProducerForAll
        gt && lt && eq


let dependencyOrderProp (CRN prog) = List.forall dependencyOrderPropStep prog

let isWellFormedCrn strict (CRN prog) =
    cmpBeforeConditionals prog
    && singleCmpForAllSteps prog
    && noLoadUseProp prog
    && validArgsProp strict prog
    && singleAssignmentForAllSteps prog
    && dependencyOrderProp (CRN prog)
