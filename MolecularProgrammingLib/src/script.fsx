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




let rec moduleToReaction =
    function
    | Ld(a, b) -> ([ ([ a ], [ a; b ], 1.0); ([ b ], [], 1.0) ], [])
    | Add(a, b, c) -> ([ ([ a ], [ a; c ], 1.0); ([ b ], [ b; c ], 1.0); ([ c ], [], 1.0) ], [])
    | Sub(a, b, c) ->
        ([ ([ a ], [ a; c ], 1.0)
           ([ b ], [ b; Species "Hsub" ], 1.0)
           ([ c ], [], 1.0)
           ([ c; Species "Hsub" ], [], 1.0) ],
         [])
    | Mul(a, b, c) -> ([ ([ a; b ], [ a; b; c ], 1.0); ([ c ], [], 1.0) ], [])
    | Div(a, b, c) -> ([ ([ a ], [ a; c ], 1.0); ([ b; c ], [ b ], 1.0) ], [])
    | Sqrt(a, b) -> ([ ([ a ], [ a; b ], 1.0); ([ b; b ], [], 0.5) ], [])
    | Cmp(x, y) ->
        (let epsilon = Species "Epsilon"
         let xplus = Species "XplusEpsilon"
         let yplus = Species "YplusEpsilon"
         let xgty = Species "XGTY"
         let xlty = Species "XLTY"
         let ygtx = Species "YGTX"
         let yltx = Species "YLTX"
         let cmpHelper = Species "CmpHelper"
         let (add1, _) = moduleToReaction (Add(x, epsilon, xplus))
         let (add2, _) = moduleToReaction (Add(y, epsilon, yplus))

         let norm =
             [ ([ xgty; y ], [ xlty; y ], 1.0)
               ([ xlty; xplus ], [ xgty; xplus ], 1.0)
               ([ ygtx; x ], [ yltx; x ], 1.0)
               ([ yltx; yplus ], [ ygtx; yplus ], 1.0) ]

         let approxMajor1 =
             [ ([ xgty; xlty ], [ xlty; cmpHelper ], 1.0)
               ([ cmpHelper; xlty ], [ xlty; xlty ], 1.0)
               ([ xlty; xgty ], [ xgty; cmpHelper ], 1.0)
               ([ cmpHelper; xgty ], [ xgty; xgty ], 1.0) ]

         let approxMajor2 =
             [ ([ ygtx; yltx ], [ yltx; cmpHelper ], 1.0)
               ([ cmpHelper; yltx ], [ yltx; yltx ], 1.0)
               ([ yltx; ygtx ], [ ygtx; cmpHelper ], 1.0)
               ([ cmpHelper; ygtx ], [ ygtx; ygtx ], 1.0) ]

         (add1 @ add2 @ norm, approxMajor1 @ approxMajor2))


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
            let rxn = ([ curr; next ], [ next; next ], 1.0)
            (next, rxn :: rxns)) (root, []) [ 0..(n-1) ] |> snd
    let inits = [("X0",1.0); ("X1",0.9)] @ (List.map (fun i ->  ($"X{i}", 1e-10)) [2..(n-1)])
    let stepCatalysts = List.map (fun i -> 3*i) [0..(steps-1)]
    (rxns,stepCatalysts,inits)

let steps = 10
let (rxns, sts, inits) = creatOscillatorReactions steps
rxns
|> simulator inits
|> Seq.take 1000
|> visualize (List.map (fun i -> $"X{i}") [0..(steps*3-1)])
printfn "%A" sts

let bindToCatalyst cat (r,p,n) = (cat::r, cat::p, n)

