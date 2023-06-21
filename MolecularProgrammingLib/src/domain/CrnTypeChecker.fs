(*
    Date: XX.06.2023
    Author: 
*)
module CrnTypeChecker

open CrnTypes


(*

    TODO: make load-use prop consider exclusivity of branches
*)


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

let getModuleOutput =
    function
    | Ld(_, b)
    | Sqrt(_, b) -> Some(b)
    | Add(_, _, c)
    | Sub(_, _, c)
    | Mul(_, _, c)
    | Div(_, _, c) -> Some(c)
    | Cmp(_) -> None

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

let rootNoLoadUseProp =
    function
    | Conc(_, _) -> true
    | Step(cmds) ->
        let (sources, loads) = getCmdsSourcesAndLoads cmds
        let overlap = Set.intersect sources loads
        (Set.isEmpty overlap)

let noLoadUseProp roots =
    roots 
        |> List.forall rootNoLoadUseProp

let checkModuleArgs =
    function
    | Ld(A, B) -> A <> B
    | Add(A, B, C) -> C <> A && C <> B
    | Sub(A, B, C) -> A <> B && C <> A && C <> B
    | Mul(A, B, C) -> A <> B && C <> A && C <> B
    | Div(A, B, C) -> C <> A && C <> B
    | Sqrt(A, B) -> A <> B
    | Cmp(A, B) -> A <> B

let checkReactionArgs (r, p, n) =
    not (List.isEmpty r && List.isEmpty p) && n > 0.0

let checkComputationArgs = 
    function
    | Mod m -> checkModuleArgs m
    | Rxn rxn -> checkReactionArgs rxn

let rec checkCommandArgs =
    function
    | Comp comp -> checkComputationArgs comp
    | Cond cond -> getConditionalComputations cond |> List.forall checkComputationArgs

let rec validArgsProp =
    function
    | [] -> true
    | Conc(_, c) :: rest -> c >= 0.0 && validArgsProp rest
    | Step(cmds) :: rest -> List.forall checkCommandArgs cmds && validArgsProp rest

let exclusivConditions cond = 
    function
    | (IfGT _) ->
        match cond with
        | (IfLT _) | (IfLE _) | (IfEQ _) -> true
        | _ -> false
    | (IfGE _) ->
        match cond with
        | (IfLT _) -> true
        | _ -> false
    | (IfEQ _) ->
        match cond with
        | (IfGT _) | (IfLT _) -> true
        | _ -> false
    | (IfLE _) ->
        match cond with
        | (IfGT _) -> true
        | _ -> false 
    | (IfLT _) ->
        match cond with
        | (IfGT _) | (IfGE _) | (IfEQ _) -> true
        | _ -> false

let moduleGetAssignment = 
    function
    | Add(a,b,c) -> [None, c]
    | Sub(a,b,c) -> [None, c]
    | Mul(a,b,c) -> [None, c]
    | Div(a,b,c) -> [None, c]
    | Sqrt(a,b) -> [None, b]
    | Ld(a,b) -> [None, b]
    | Cmp(a,b) -> []

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
    | Rxn(r,p,n) ->
        let species = List.distinct (r @ p)
        let nonCats = List.filter (fun sp -> netChange sp (r,p,n) <> 0) species
        nonCats |> List.map (fun sp -> None, sp)

let conditionalAssignments cond = 
    cond 
        |> getConditionalComputations 
        |> List.collect computationAssignments 
        |> List.map (fun (_,sp) -> Some(cond), sp)

let rec forAllUniquePairs pred acc = function
    | [] | [_] -> acc
    | x::xs ->
        let res = List.forall (pred x) xs
        forAllUniquePairs pred (acc && res) xs

let commandAssignments = 
    function
    | Comp c -> computationAssignments c
    | Cond c -> conditionalAssignments c

let stepAssignments = 
    function
    | Conc(_,_) -> []
    | Step cmds -> List.map commandAssignments cmds |> List.concat


let noAssignmentCollision (c0, sp0) (c1,sp1) =
    match (sp0 = sp1,c0,c1) with
    | (false,_,_) -> true
    | (true,Some(con0),Some(con1)) when exclusivConditions con0 con1 -> true
    | _ -> false




let singleAssignmentsPerStep (root) = 
    root
        |> stepAssignments
        |> forAllUniquePairs noAssignmentCollision true
            

let singleAssignmentForAllSteps roots = 
    roots
        |> List.forall singleAssignmentsPerStep

let computationCmpCount = function
    | Mod(Cmp(_,_)) -> None, true
    | _ -> None, false

let commandCmpCount = function
    | Comp c -> [computationCmpCount c]
    | Cond c ->
        getConditionalComputations c
            |> List.map computationCmpCount
            |> List.map (fun (_,cnt) -> Some c, cnt)
        
let stepCmpCount = function
    | Conc(_,_) -> []
    | Step(cmds) ->
        cmds 
            |> List.collect commandCmpCount

let noCmpCollision (c0, isBr0) (c1, isBr1) =
    match (isBr0,isBr1,c0,c1) with
    | (false,_,_,_) -> true
    | (_,false,_,_) -> true
    | (true,true,Some(con0),Some(con1)) when exclusivConditions con0 con1 -> true
    | _ -> false
    
let singleCmpPerStep root = 
    root
        |> stepCmpCount
        |> forAllUniquePairs noCmpCollision true
            

let singleCmpForAllSteps roots = 
    roots
        |> List.forall singleCmpPerStep



let isWellFormedCrn (CRN prog) =
    cmpBeforeConditionals prog
    && noLoadUseProp prog
    && validArgsProp prog
    && singleAssignmentForAllSteps prog
    && singleCmpForAllSteps prog
