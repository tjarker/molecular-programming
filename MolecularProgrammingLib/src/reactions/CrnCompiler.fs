(*
    Date: XX.06.2023
    Author: 
*)
module CrnCompiler

open CrnTypes

let subHelper = "SubHelper"
let epsilon = Species "Epsilon"
let cmpOffset = "CmpOffset"
let xgty = Species "XGTY"
let xlty = Species "XLTY"
let ygtx = Species "YGTX"
let yltx = Species "YLTX"
let cmpHelper = "CmpHelper"
let oscillatorSeries = "X"

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
        @ (List.map (fun i -> ($"X{i}", 1e-5)) [ 1 .. (n - 2) ])

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

let compile speed (CRN roots) =
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
    let enumerate = List.mapi (fun i x -> (i, x))

    let (helperInits, rxns) =
        List.fold
            (fun (initMap, rxns) (i, step) ->
                let (_, initMap', rxns') = stepToReactions i step initMap
                (initMap', rxns @ rxns'))
            (Map.empty, clockRxns)
            (enumerate steps)

    let zeroInits = scanForSpecies rxns |> List.map (fun sp -> (sp, 0.0)) |> Map.ofList

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

let reactionsPrettyPrint prog =
    for rxn: Species list * Species list * float in (prog |> (compile 1.0) |> fst) do
        printfn "%s" (reactionPrettyFormat rxn)
