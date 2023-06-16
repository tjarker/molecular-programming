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

printfn "Molecular Programming Library"

let HSub = Species "HSub"
let epsilon = Species "Epsilon"
let xplus = Species "XplusEpsilon"
let yplus = Species "YplusEpsilon"
let xgty = Species "XGTY"
let xlty = Species "XLTY"
let ygtx = Species "YGTX"
let yltx = Species "YLTX"
let cmpHelperXY = Species "CmpHelperXY"
let cmpHelperYX = Species "CmpHelperYX"

let bindToCatalyst (r,p,n) cat= (cat::r, cat::p, n)

let bindToCatalysts cats rxns = 
    List.map (fun rxn -> List.fold bindToCatalyst rxn cats) rxns

let rec moduleToReaction step cats =
    let c0 = Species $"X{step}"
    let c1 = Species $"X{step+1}"
    function
    | Ld(a, b) -> 
        [
            ([ a ], [ a; b ], 1.0); 
            ([ b ], [], 1.0) 
        ] 
        |> bindToCatalysts (c0::cats)

    | Add(a, b, c) -> 
        [ 
            ([ a ], [ a; c ], 1.0); 
            ([ b ], [ b; c ], 1.0); 
            ([ c ], [], 1.0) 
        ]
        |> bindToCatalysts (c0::cats)

    | Sub(a, b, c) ->
        [ 
            ([ a ], [ a; c ], 1.0);
            ([ b ], [ b; HSub ], 1.0);
            ([ c ], [], 1.0);
            ([ c; HSub ], [], 1.0) 
        ]
        |> bindToCatalysts (c0::cats)
        
    | Mul(a, b, c) -> 
        [ 
            ([ a; b ], [ a; b; c ], 1.0); 
            ([ c ], [], 1.0) 
        ]
        |> bindToCatalysts (c0::cats)

    | Div(a, b, c) -> 
        [ 
            ([ a ], [ a; c ], 1.0); 
            ([ b; c ], [ b ], 1.0) 
        ]
        |> bindToCatalysts (c0::cats)

    | Sqrt(a, b) -> 
        [ 
            ([ a ], [ a; b ], 1.0); 
            ([ b; b ], [], 0.5) 
        ]
        |> bindToCatalysts (c0::cats)

    | Cmp(x, y) ->          
        let add1 = moduleToReaction step cats (Add(x, epsilon, xplus))
        let add2 = moduleToReaction step cats (Add(y, epsilon, yplus))
        let norm =
            [ ([ xgty; y ], [ xlty; y ], 1.0)
              ([ xlty; xplus ], [ xgty; xplus ], 1.0)
              ([ ygtx; x ], [ yltx; x ], 1.0)
              ([ yltx; yplus ], [ ygtx; yplus ], 1.0) ]
        let approxMajor1 =
            [ ([ xgty; xlty ], [ xlty; cmpHelperXY ], 1.0)
              ([ cmpHelperXY; xlty ], [ xlty; xlty ], 1.0)
              ([ xlty; xgty ], [ xgty; cmpHelperXY ], 1.0)
              ([ cmpHelperXY; xgty ], [ xgty; xgty ], 1.0) ]
        let approxMajor2 =
            [ ([ ygtx; yltx ], [ yltx; cmpHelperYX ], 1.0)
              ([ cmpHelperYX; yltx ], [ yltx; yltx ], 1.0)
              ([ yltx; ygtx ], [ ygtx; cmpHelperYX ], 1.0)
              ([ cmpHelperYX; ygtx ], [ ygtx; ygtx ], 1.0) ]
        let thisStep = (add1 @ add2 @ norm) |> bindToCatalysts (c0::cats)
        let nextStep = (approxMajor1 @ approxMajor2) |> bindToCatalysts (c1::cats)
        thisStep @ nextStep
         


// (Add(Species "A", Species "B", Species "C"))
// |> moduleToReaction
// |> fst
// |> simulator [ ("A", 6.0); ("B", 2.0); ("C", 0.0) ]
// |> Seq.take 6000
// |> visualize [ "A"; "B"; "C" ]

// (Sub(Species "A", Species "B", Species "C"))
// |> moduleToReaction
// |> fst
// |> simulator [ ("A", 6.0); ("B", 2.0); ("C", 0.0) ]
// |> Seq.take 6000
// |> visualize [ "A"; "B"; "C"; "Hsub" ]

// (Mul(Species "A", Species "B", Species "C"))
// |> moduleToReaction
// |> fst
// |> simulator [ ("A", 6.0); ("B", 2.0); ("C", 0.0) ]
// |> Seq.take 6000
// |> visualize [ "A"; "B"; "C" ]

// (Div(Species "A", Species "B", Species "C"))
// |> moduleToReaction
// |> fst
// |> simulator [ ("A", 6.0); ("B", 2.0); ("C", 0.0) ]
// |> Seq.take 6000
// |> visualize [ "A"; "B"; "C" ]

// (Ld(Species "A", Species "B"))
// |> moduleToReaction
// |> fst
// |> simulator [ ("A", 6.0); ("B", 2.0) ]
// |> Seq.take 6000
// |> visualize [ "A"; "B" ]

// (Sqrt(Species "A", Species "B"))
// |> moduleToReaction
// |> fst
// |> simulator [ ("A", 16.0); ("B", 0.0) ]
// |> Seq.take 6000
// |> visualize [ "A"; "B" ]

// (Cmp(Species "A", Species "B"))
// |> moduleToReaction
// |> fst
// |> simulator [ ("A", 2.0); ("B", 4.0) ]
// |> Seq.take 10000
// |> visualize [ "A"; "B"; "XGTY"; "XLTY"; "YGTX"; "YLTX"; "XplusEpsilon"; "YplusEpsilon" ]

// (Cmp(Species "A", Species "B"))
// |> moduleToReaction
// |> snd
// |> simulator
//     [ ("A", 2.0)
//       ("B", 4.0)
//       ("XGTY", 0.38)
//       ("XLTY", 0.62)
//       ("YGTX", 0.69)
//       ("YLTX", 0.3) ]
// |> Seq.take 10000
// |> visualize
//     [ "A"
//       "B"
//       "XGTY"
//       "XLTY"
//       "YGTX"
//       "YLTX"
//       "XplusEpsilon"
//       "YplusEpsilon"
//       "CmpHelper" ]



let creatOscillatorReactions (steps: int) =
    assert (steps > 0)
    let n = steps * 3
    let root = Species "X0"
    let rxns = List.fold (fun (curr, rxns) i ->
            let next = if i < (n-1) then Species $"X{i + 1}" else root
            let rxn = ([ curr; next ], [ next; next ], 3.5)
            (next, rxn :: rxns)) (root, []) [ 0..(n-1) ] |> snd
    let inits = [("X0",0.9); ($"X{n-1}",1.0)] @ (List.map (fun i ->  ($"X{i}", 1e-10)) [1..(n-2)])
    (rxns,inits |> List.map (fun (n,v) -> (Species n,v)))

// let steps = 10
// let (rxns, sts, inits) = creatOscillatorReactions steps
// rxns
// |> simulator inits
// |> Seq.take 1000
// |> visualize (List.map (fun i -> $"X{i}") [0..(steps*3-1)])
// printfn "%A" sts



let computationToReactions step cats= 
    let c0 = Species $"X{step}"
    function
    | Mod(m) -> moduleToReaction step cats m
    | Rxn(rxn) -> [rxn] |> bindToCatalysts (c0::cats)

let getConditionalComputations =
    function
    | IfGT(comps) -> comps
    | IfGE(comps) -> comps
    | IfEQ(comps) -> comps
    | IfLT(comps) -> comps
    | IfLE(comps) -> comps

let conditionalsCatalysts = 
    function
    | IfGT(_) -> [xgty; yltx]
    | IfGE(_) -> [xgty]
    | IfEQ(_) -> [xgty; ygtx] 
    | IfLE(_) -> [ygtx]
    | IfLT(_) -> [xlty; ygtx]

let conditionalToReactions step cond = 
    let cats = conditionalsCatalysts cond
    getConditionalComputations cond 
    |> List.map (computationToReactions step cats)
    |> List.concat

let commandToReactions step = 
    function
    | Comp c -> computationToReactions step [] c
    | Cond c -> conditionalToReactions step c

let stepToReactions n step =
    List.map (commandToReactions (n*3)) step |> List.concat

let scanForSpecies rxns = List.fold (fun set (r,p,_) -> Set.unionMany [set; Set r; Set p]) Set.empty rxns |> Set.toList

let crnToReactions (CRN roots) =
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

    let rxns = List.mapi stepToReactions steps |> List.concat

    
    let zeroInits = scanForSpecies rxns |> List.map (fun sp -> (sp,0.0)) |> Map.ofList
    let (clockRxns, clockInits) = creatOscillatorReactions stepCount
    let defaultSpecies =
        [ ("Hsub", 0.0)
          ("Epsilon", 0.5)
          ("XplusEpsilon", 0.0)
          ("YplusEpsilon", 0.0)
          ("XGTY", 0.5)
          ("XLTY", 0.5)
          ("YGTX", 0.5)
          ("YLTX", 0.5)
          ("CmpHelperXY", 0.0); ("CmpHelperYX",0.0) ] |> List.map (fun (n, c) -> (Species n, c))

    let init = List.fold (fun init (s,v) -> Map.add s v init) zeroInits (clockInits @ defaultSpecies @ concs)
    
    (
        rxns @ clockRxns,
        init
    )
        


let reactionPrettyPrinter (r,p,n) =
    let formatSpeciesList sps = List.map (fun (Species s) -> s) sps |> String.concat " + "
    sprintf "%s -> %s" (formatSpeciesList r) (formatSpeciesList p) 



counter |> parse |> crnToReactions |> simulator |> Seq.take 10000 |> visualize ["c";"cnext";"X0";"X1";"X3";"XGTY";"XLTY";"YLTX";"YGTX"]