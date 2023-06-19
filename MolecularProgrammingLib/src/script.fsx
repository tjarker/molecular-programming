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

let compareStates tol ((State(mo1, n1, flags1)), (State(mo2, n2, flags2))) =
    let m1 = filterSpeciesMap mo1
    let m2 = filterSpeciesMap mo2
    let res = (compareStateMaps m1 m2 tol)

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

let validate prog numStates tolerance =
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

    printf "%A" clockSpecies
    let compiledStates = prog |> (compile 1.0) |> simulator

    let compiledStates =
        Seq.cache (
            seq {
                yield Seq.head compiledStates
                yield! compiledStates |> findPeaks numStates clockSpecies
            }
        )

    compareSeqStates interpretedStates compiledStates tolerance

//printfn "%O" (validate (gcd |> parse) 4 0.1)

let everyNth n seq = 
    seq |> Seq.mapi (fun i el -> el, i)              // Add index to element
        |> Seq.filter (fun (el, i) -> i % n = 0) // Take every nth element
        |> Seq.map fst    

//gcd |> parse |> compile 1.0 |> simulator |> Seq.take 100000 |> everyNth 70 |> visualize ["a";"b";"X0";"X3"]



printfn "%A" (Gen.sample 4 1 (stepGen [Species "A";Species "B";Species "C";Species "D";Species "E";Species "F";Species "G"]) |> List.head)