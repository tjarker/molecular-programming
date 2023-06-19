(*
    Date: XX.06.2023
    Author: 
*)
#if INTERACTIVE
// A relative path to MolecularProgrammingLib.dll
#r @"../bin/Debug/net6.0/MolecularProgrammingLib.dll"
#r @"../bin/Debug/net6.0/DrawingTreesLib.dll"
#r "nuget: FParsec"
#r "nuget: FsCheck"
#r "nuget: Plotly.NET"
#endif

open CrnTypes
open CrnString
open CrnParser
open CrnInterpreter
open CrnSimulator
open CrnExamples
open CrnVisualizer
open CrnSimulator
open CrnGenerator
open CrnCompiler
open FsCheck

printfn "Molecular Programming Library"



// let (rnxs,init) = counter |> parse |> crnToReactions

// for i in init do printfn "%O" i

// for rxn in (counter |> parse |> crnToReactions |> fst) do printfn "%O" (reactionPrettyPrinter rxn)

// let subCrn = CRN [ Conc(Species "A", 5.0); Conc(Species "B", 2.0); Step([ Comp(Mod(Sub(Species "A", Species "B", Species "C")))])]
// for rxn in (subCrn |> crnToReactions |> fst) do printfn "%O" (reactionPrettyPrinter rxn)


let counterL = counter, [ "c"; "cnext" ]
let gcdL = gcd, [ "a"; "b"; "X0"; "X3" ]
let piApproxL = piApprox, [ "pi"; "divisor1"; "divisor2" ]
let eulerApproxL = eulerApprox, [ "e" ]
let integerSqrtL = integerSqrt, [ "n"; "z"; "zpow"; "out"; "znext"; "X0"; "X3" ]

// gcd |> parse |> crnToReactions 1.0 |> simulator |> Seq.take 8000 |> visualize [ "a"; "b"; "X0"; "X3"; "X1"; "X2" ]

let square =
    ([ ([ Species "A"; Species "B" ], [ Species "A"; Species "B"; Species "C" ], 1.0)
       ([ Species "C" ], [], 1.0) ],
     Map [ (Species "A", 2.0); (Species "B", 2.0); (Species "C", 0.0) ])

// Compare state maps
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

let compareStates (State(m1, n1, f1)) (State(m2, n2, f2)) tol =
    let m1 = filterSpeciesMap m1
    let m2 = filterSpeciesMap m2
    printfn "-----------------"
    State.prettyPrint (State(m1, n1, f1))
    printfn "-----------------"
    State.prettyPrint (State(m2, n2, f2))
    (compareStateMaps m1 m2 tol) && n1 = n2 && f1 = f2

// Compare a sequence of states to determine if they are the same
let rec compareSeqStates seq1 seq2 tol =
    if Seq.isEmpty seq1 || Seq.isEmpty seq2 then
        (Seq.isEmpty seq1) && (Seq.isEmpty seq2)
    elif not (compareStates (Seq.head seq1) (Seq.head seq2) tol) then
        false
    else
        compareSeqStates (Seq.skip 1 seq1) (Seq.skip 1 seq2) tol

let validate prog numStates tolerance =
    let interpretedStates = interpreter prog |> Seq.take numStates

    let compiledStates =
        prog |> (crnToReactions 10.0) |> simulator |> Seq.take numStates

    compareSeqStates interpretedStates compiledStates tolerance

// validate (parse counter) 2 0.1

// let interpretedStates = counter |> parse |> interpreter

// let compiledStates = counter |> parse |> (crnToReactions 1.0) |> simulator

// let numStates = 40

// for (State(m1, n1, f1), State(m2, n2, f2)) in Seq.zip interpretedStates compiledStates |> Seq.take numStates do
//     printfn "---------------------"
//     State.prettyPrint (State(filterSpeciesMap m1, n1, f1))
//     printfn "---------------------"
//     State.prettyPrint (State(filterSpeciesMap m2, n2, f2))

// let sqrtCrn =
//     CRN [ Conc(Species "A", 9.0); Step([ Comp(Mod(Sqrt(Species "A", Species "B"))) ]) ]

// sqrtCrn
// |> crnToReactions (1.0)
// |> simulator
// |> Seq.take 1000
// |> visualize [ "A"; "B" ]

let samplingThreshold = 1.8

let nextPeakCenter species states =
    let fromStart =
        Seq.skipWhile (fun (State(env, _, _)) -> (Map.find species env) < samplingThreshold) states

    let (State(_, start, _)) = Seq.head fromStart

    let fromStop =
        Seq.skipWhile (fun (State(env, _, _)) -> (Map.find species env) >= samplingThreshold) fromStart
    
    let (State(_, stop, _)) = Seq.head fromStop
    let center = stop - start
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

    peaks


let samplePeaks states =
    seq {
        for (State(env, x, _)) in states do
            yield (Map.find (Species "X2") env)
    }

let states = gcd |> parse |> crnToReactions 1.0 |> simulator

printfn "%O" (findPeaks 5 [ Species "X2"; Species "X5" ] states)
