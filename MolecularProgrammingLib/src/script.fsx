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
open CrnTypeChecker

printfn "Molecular Programming Library"

(*
    TODO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    - check in interpreter that command list order is irrelevant

*)

// let (rnxs,init) = counter |> parse |> crnToReactions

// for i in init do printfn "%O" i

// for rxn in (counter |> parse |> crnToReactions |> fst) do printfn "%O" (reactionPrettyPrinter rxn)

// let subCrn = CRN [ Conc(Species "A", 5.0); Conc(Species "B", 2.0); Step([ Comp(Mod(Sub(Species "A", Species "B", Species "C")))])]
// for rxn in (subCrn |> crnToReactions |> fst) do printfn "%O" (reactionPrettyPrinter rxn)

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

let validate numStates tolerance prog =
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

    let compiledStates = prog |> (compile 1.0) |> simulator 0.04

    let compiledStates =
        Seq.cache (
            seq {
                yield Seq.head compiledStates
                yield! compiledStates |> findPeaks numStates clockSpecies
            }
        )

    compareSeqStates interpretedStates compiledStates tolerance

// simPlot 0.04 170.0 (parse gcd) ["a";"b";"X0";"X3"]



CrnGenerator.initialize ()

Check.Quick (validate 4 0.1)


(*
    State Mismatch -----------------
State 2:
        Iuk = 4.58444741903007, Mj = 0.7883232963234288, Y = 5.255745322775859, a = 8.87565920558094, hOy = 3.813773178337433, lr = 2.626755393850674, uNOo = 1.7514252243329256, vKp = 3.905691414095768, xO = 4.747032836430687
        (False, False)
--------------------------------
State 917:
        Iuk = 4.58444741903007, Mj = 0.7883232963234288, Y = 5.255537819532248, a = 8.87565920558094, hOy = 3.813773178337433, lr = 2.626755393850674, uNOo = 1.7514252243329256, vKp = 3.905691414095768, xO = 1.17854278285377
        (False, False)
State Mismatch -----------------
*)
let failing = CRN [
    Conc (Species "Mj", 0.7883232963); Conc (Species "xO", 4.747032836);
   Conc (Species "Y", 5.255745323); Conc (Species "uNOo", 1.751425224);
   Conc (Species "hOy", 3.813773178); Conc (Species "Iuk", 4.584447419);
   Conc (Species "lr", 2.626755394); Conc (Species "vKp", 3.905691414);
   Conc (Species "a", 8.875659206);
   Step [Comp (Mod (Cmp (Species "vKp", Species "Y")))];
   Step [Cond (IfGE [Mod (Div (Species "Mj", Species "xO", Species "xO"))])];
   Step [Cond (IfGE [Mod (Mul (Species "Mj", Species "hOy", Species "Y"))])]]