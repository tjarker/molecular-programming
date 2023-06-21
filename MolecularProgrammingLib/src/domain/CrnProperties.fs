module CrnProperties

open FsCheck
open CrnTypes
open CrnInterpreter
open CrnSimulator
open CrnCompiler

let shuffle xs = 
    Gen.shuffle xs |> Gen.map Array.toList |> Gen.sample 0 1 |> List.head

let shuffleConditionalAssignments = 
    function
    | IfGT comps -> IfGT (shuffle comps)
    | IfGE comps -> IfGE (shuffle comps)
    | IfEQ comps -> IfEQ (shuffle comps)
    | IfLE comps -> IfLE (shuffle comps)
    | IfLT comps -> IfLT (shuffle comps)

let shuffleCommands =
    function
    | Comp c -> Comp c
    | Cond c -> Cond (shuffleConditionalAssignments c)

let shuffleAssignments (CRN roots) = 
    roots |> List.map 
        (function
        | Conc(sp,c) -> Conc(sp,c)
        | Step(coms) -> 
            coms
                |> List.map shuffleCommands
                |> shuffle
                |> Step) |> CRN

let orderDoesntMatterProp steps crn =
    let shuffledCrn = shuffleAssignments crn
    let oriSeq = crn |> interpreter
    let shufSeq = shuffledCrn |> interpreter
    Seq.zip oriSeq shufSeq
        |> Seq.take steps
        |> Seq.forall (fun (s0,s1) -> s0 = s1)





let compareStateMaps map1 map2 tol =
    Map.forall
        (fun sp1 v1 -> // TODO: Maybe we should ignore helper variables?
            match Map.tryFind sp1 map2 with
            | Some(v2) -> abs (v1 - v2) < tol
            | None -> false)
        map1

let filterSpeciesMap map =
    let speciesList = [ xgty; xlty; ygtx; yltx; epsilon ]
    let enumeratedLbls = [ subHelper; cmpHelper; cmpOffset; oscillatorSeries ]

    Map.filter
        (fun (Species(sp: string)) _ ->
            not (List.contains (Species sp) speciesList)
            && not (List.exists (fun (lbl: string) -> sp.StartsWith(lbl)) enumeratedLbls))
        map

let compareStates tol ((State(mo1, n1, flags1)), (State(mo2, n2, flags2))) =
    let m1 = filterSpeciesMap mo1
    let m2 = filterSpeciesMap mo2
    let res = (compareStateMaps m1 m2 tol)

    printfn $"Comparing interpreter state {n1} with simulator state {n2}..."

    if not res then
        printfn "State Mismatch -----------------"
        State.prettyPrint (State(m1, n1, flags1))
        printfn "--------------------------------"
        State.prettyPrint (State(m2, n2, flags2))
        printfn "State Mismatch -----------------"

    res

// Compare a sequence of states to determine if they are the same
let compareSeqStates seq1 seq2 tol =
    Seq.forall (compareStates tol) (Seq.zip seq1 seq2)

let samplingThreshold = 1.8

let nextPeakCenter species states =

    let fromStart =
        Seq.skipWhile (fun (State(env, _, _)) -> (Map.find species env) < samplingThreshold) states

    let (State(_, start, _)) = Seq.head fromStart

    let fromStop =
        Seq.skipWhile (fun (State(env, _, _)) -> (Map.find species env) >= samplingThreshold) fromStart

    let (State(_, stop, _)) = Seq.head fromStop

    let center = (stop - start) / 2
    let centerState = fromStart |> Seq.skip center |> Seq.head
    centerState, fromStop

let findPeaks n clocks states =
    let (peaks, _, _) =
        (List.fold
            (fun (peaks, clocks, stop) _ ->
                let c = List.head clocks
                let cs = List.tail clocks
                let (center, nextStop) = nextPeakCenter c stop
                (center :: peaks, cs @ [ c ], nextStop))
            ([], clocks, states)
            [ 1..n ])

    List.rev peaks

let rec countPred pred acc = function 
    | [] -> acc
    | x::xs -> 
        let nextAcc = if pred x then acc+1 else acc
        countPred pred nextAcc xs

let validate tolerance prog =
    printfn "Entering Validate Prop"

    let (CRN roots) = prog

    let numStates = 
        countPred (
            function
            | Conc(_,_) -> false
            | Step(_) -> true
        ) 0 roots

    let interpretedStates = interpreter prog |> Seq.take numStates
    let (CRN roots) = prog

    let numSteps =
        List.length (
            List.choose
                (fun root ->
                    match root with
                    | Step(_) -> Some(root)
                    | _ -> None)
                roots
        )

    let clockSpecies =
        List.map (fun i -> Species $"X{3 * i + 2}") [ 0 .. (numSteps - 1) ]

    let compiledStates = prog |> (compile 0.5) |> simulator 0.04

    let compiledStates =
        Seq.cache (
            seq {
                yield Seq.head compiledStates
                yield! compiledStates |> findPeaks numStates clockSpecies
            }
        )

    let res = compareSeqStates interpretedStates compiledStates tolerance
    printfn "Exiting Validate Prop"
    res