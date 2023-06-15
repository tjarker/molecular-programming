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

printfn "Molecular Programming Library"

counter |> parse |> interpreter |> Seq.take 10 |> visualize [ "c"; "cnext" ]

crn3
|> simulator [ ("A", 6.0); ("B", 2.0); ("C", 0.0) ]
|> Seq.take 6000
|> visualize [ "A"; "B"; "C" ]

crn4
|> simulator [ ("A", 6.0); ("B", 2.0); ("C", 0.0) ]
|> Seq.take 6000
|> visualize [ "A"; "B"; "C" ]
