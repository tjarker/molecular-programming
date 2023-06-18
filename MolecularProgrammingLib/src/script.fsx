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
open FsCheck

printfn "Molecular Programming Library"

let subHelper = "SubHelper"
let epsilon = Species "Epsilon"
let cmpOffset = "CmpOffset"
let xgty = Species "XGTY"
let xlty = Species "XLTY"
let ygtx = Species "YGTX"
let yltx = Species "YLTX"
let cmpHelper = "CmpHelper"

let bindToCatalyst (r, p, n) cat = (cat :: r, cat :: p, n)

let bindToCatalysts cats rxns =
    List.map (fun rxn -> List.fold bindToCatalyst rxn cats) rxns

let getNextHelper helperName helperMap =
    let helperId =
        match Map.tryFind helperName helperMap with
        | Some n -> n
        | None -> 0

    let newHelperMap = Map.add helperName (helperId + 1) helperMap
    let helper = Species(helperName + (helperId.ToString()))
    (helper, newHelperMap)

let rec compare step cats helperMap initMap sp1 sp2 sp1gtsp2 sp1ltsp2 offset =
    let bindToLocalContext = bindToCatalysts ((Species $"X{step}") :: cats)
    let bindToNextContext = bindToCatalysts ((Species $"X{step + 1}") :: cats)
    let (offsetHelper, helperMap') = getNextHelper cmpOffset helperMap
    let (cmpHelper, helperMap'') = getNextHelper cmpHelper helperMap'

    let (_, _, add) =
        moduleToReaction step cats Map.empty Map.empty (Add(sp1, offset, offsetHelper))

    let norm =
        [ ([ sp1gtsp2; sp2 ], [ sp1ltsp2; sp2 ], 1.0)
          ([ sp1ltsp2; offsetHelper ], [ sp1gtsp2; offsetHelper ], 1.0) ]

    let approxMajor =
        [ ([ sp1gtsp2; sp1ltsp2 ], [ sp1ltsp2; cmpHelper ], 1.0)
          ([ cmpHelper; sp1ltsp2 ], [ sp1ltsp2; sp1ltsp2 ], 1.0)
          ([ sp1ltsp2; sp1gtsp2 ], [ sp1gtsp2; cmpHelper ], 1.0)
          ([ cmpHelper; sp1gtsp2 ], [ sp1gtsp2; sp1gtsp2 ], 1.0) ]

    let thisStep = (add @ norm) |> bindToLocalContext
    let nextStep = approxMajor |> bindToNextContext

    let initMap' =
        List.fold
            (fun acc (s, v) -> Map.add s v acc)
            initMap
            [ (offsetHelper, 0.0); (sp1gtsp2, 0.5); (sp1ltsp2, 0.5); (cmpHelper, 0.0) ]

    (helperMap'', initMap', thisStep @ nextStep)

and moduleToReaction step cats helperMap initMap =
    let bindToLocalContext = bindToCatalysts ((Species $"X{step}") :: cats)

    function
    | Ld(a, b) ->
        let rxn = [ ([ a ], [ a; b ], 1.0); ([ b ], [], 1.0) ]
        (helperMap, initMap, rxn |> bindToLocalContext)

    | Add(a, b, c) ->
        let rxn = [ ([ a ], [ a; c ], 1.0); ([ b ], [ b; c ], 1.0); ([ c ], [], 1.0) ]
        (helperMap, initMap, rxn |> bindToLocalContext)
    | Sub(a, b, c) ->
        let (helper, newHelperMap) = getNextHelper subHelper helperMap
        let newInitMap = Map.add helper 0.0 initMap

        let rxn =
            [ ([ a ], [ a; c ], 1.0)
              ([ b ], [ b; helper ], 1.0)
              ([ c ], [], 1.0)
              ([ c; helper ], [], 1.0) ]

        (newHelperMap, newInitMap, rxn |> bindToLocalContext)

    | Mul(a, b, c) ->
        let rxn = [ ([ a; b ], [ a; b; c ], 1.0); ([ c ], [], 1.0) ]
        (helperMap, initMap, rxn |> bindToLocalContext)

    | Div(a, b, c) ->
        let rxn = [ ([ a ], [ a; c ], 1.0); ([ b; c ], [ b ], 1.0) ]
        (helperMap, initMap, rxn |> bindToLocalContext)

    | Sqrt(a, b) ->
        let rxn = [ ([ a ], [ a; b ], 1.0); ([ b; b ], [], 0.5) ]
        (helperMap, initMap, rxn |> bindToLocalContext)

    | Cmp(x, y) ->
        let (helperMap', initMap', firstCmp) =
            compare step cats helperMap initMap x y xgty xlty epsilon

        let (helperMap'', initMap'', secondCmp) =
            compare step cats helperMap' initMap' y x ygtx yltx epsilon

        (helperMap'', initMap'', firstCmp @ secondCmp)

let creatOscillatorReactions speed (steps: int) =
    assert (steps > 0)
    let n = steps * 3
    let root = Species "X0"

    let rxns =
        List.fold
            (fun (curr, rxns) i ->
                let next = if i < (n - 1) then Species $"X{i + 1}" else root
                let rxn = ([ curr; next ], [ next; next ], speed)
                (next, rxn :: rxns))
            (root, [])
            [ 0 .. (n - 1) ]
        |> snd

    let inits =
        [ ("X0", 0.9); ($"X{n - 1}", 1.0) ]
        @ (List.map (fun i -> ($"X{i}", 1e-10)) [ 1 .. (n - 2) ])

    (rxns, inits |> List.map (fun (n, v) -> (Species n, v)))

let computationToReactions step cats helperMap initMap =
    let c0 = Species $"X{step}"

    function
    | Mod(m) -> moduleToReaction step cats helperMap initMap m
    | Rxn((p, r, _) as rxn) -> (Map.empty, Map.empty, [ rxn ] |> bindToCatalysts (c0 :: cats))

let getConditionalComputations =
    function
    | IfGT(comps) -> comps
    | IfGE(comps) -> comps
    | IfEQ(comps) -> comps
    | IfLT(comps) -> comps
    | IfLE(comps) -> comps

let conditionalsCatalysts =
    function
    | IfGT(_) -> [ xgty; xgty; yltx; yltx ]
    | IfGE(_) -> [ xgty ]
    | IfEQ(_) -> [ xgty; ygtx ]
    | IfLE(_) -> [ ygtx ]
    | IfLT(_) -> [ xlty; xlty; ygtx; ygtx ]

let conditionalToReactions step cond helperMap initMap =
    let cats = conditionalsCatalysts cond

    getConditionalComputations cond
    |> List.fold
        (fun (helperMap', initMap', rxns) comp ->
            let (helperMap', initMap', newRxns) =
                computationToReactions step cats helperMap' initMap' comp

            (helperMap', initMap', newRxns @ rxns))
        (helperMap, initMap, [])

let commandToReactions step helperMap initMap =
    function
    | Comp c -> computationToReactions step [] helperMap initMap c
    | Cond c -> conditionalToReactions step c helperMap initMap

let stepToReactions n step initMap =
    List.fold
        (fun (helperMap, initMap', rxns) com ->
            let (helperMap', initMap'', newRxns) =
                commandToReactions (n * 3) helperMap initMap' com

            (helperMap', initMap'', rxns @ newRxns))
        (Map.empty, initMap, [])
        step

let scanForSpecies rxns =
    List.fold (fun set (r, p, _) -> Set.unionMany [ set; Set r; Set p ]) Set.empty rxns
    |> Set.toList

let enumerate = List.mapi (fun i x -> (i, x))

let crnToReactions speed (CRN roots) =
    let concs =
        roots
        |> List.choose (function
            | (Conc(s, c)) -> Some(s, c)
            | _ -> None)

    let steps =
        roots
        |> List.choose (function
            | (Step cmds) -> Some(cmds)
            | _ -> None)

    let stepCount = List.length steps

    let (clockRxns, clockInits) = creatOscillatorReactions speed stepCount

    let (helperInits, rxns) =
        List.fold
            (fun (initMap, rxns) (i, step) ->
                let (_, initMap', rxns') = stepToReactions i step initMap
                (initMap', rxns @ rxns'))
            (Map.empty, clockRxns)
            (enumerate steps)

    let zeroInits = scanForSpecies rxns |> List.map (fun sp -> (sp, 0.0)) |> Map.ofList

    // printfn "%A" helperInits
    // printfn "%A" clockInits
    // printfn "%A" concs
    // for init in  zeroInits do printfn "%O" init


    let init =
        List.fold
            (fun init (s, v) -> Map.add s v init)
            zeroInits
            (Map.toList helperInits @ clockInits @ (((Species "Epsilon", 0.5) :: concs)))

    (rxns, init)

let reactionPrettyFormat (r, p, n) =
    let speciesListFormat sps =
        List.map (fun (Species s) -> s) sps |> String.concat " + "

    sprintf "%s -> %s" (speciesListFormat r) (speciesListFormat p)

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

let reactionsPrettyPrint prog =
    for rxn in (prog |> parse |> (crnToReactions 1.0) |> fst) do
        printfn "%s" (reactionPrettyFormat rxn)

// Compare state maps
let compareStateMaps map1 map2 tol =
    Map.forall
        (fun sp1 v1 -> // TODO: Maybe we should ignore helper variables?
            match Map.tryFind sp1 map2 with
            | Some(v2) -> abs (v1 - v2) < tol
            | None -> false)
        map1

let compareStates (State(m1, n1, f1)) (State(s2, n2, f2)) tol =
    printfn "-----------------"
    State.prettyPrint (State(m1, n1, f1))
    printfn "-----------------"
    State.prettyPrint (State(s2, n2, f2))
    (compareStateMaps m1 s2 tol) && n1 = n2 && f1 = f2

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

validate (parse counter) 2 0.1
