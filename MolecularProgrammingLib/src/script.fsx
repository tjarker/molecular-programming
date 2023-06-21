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
open CrnProperties

printfn "Molecular Programming Library"

CrnGenerator.initialize ()

let failing = CRN [Conc (Species "c", 0.6525157659); Conc (Species "Xm", 3.622123494);
   Conc (Species "zOtI", 7.93367376); Conc (Species "eJ", 8.172075734);
   Conc (Species "ZoTi", 0.4163264276); Conc (Species "eJ", 1.465519137);
   Conc (Species "Ty", 4.435126866); Conc (Species "oDiX", 7.386363289);
   Conc (Species "Q", 9.003783396); Conc (Species "VaP", 1.154198307);
   Conc (Species "q", 3.894631611); Conc (Species "VuZ", 6.602731802);
   Conc (Species "k", 8.127802409); Conc (Species "F", 8.281071796);
   Conc (Species "hWb", 8.137609182); Conc (Species "m", 4.64956357);
   Conc (Species "Hw", 7.357664021); Conc (Species "wLqF", 7.468080833);
   Conc (Species "Bg", 9.128430786); Conc (Species "wIxC", 0.8616499287);
   Conc (Species "Yn", 3.778064707); Conc (Species "Di", 8.095535904);
   Conc (Species "yNsH", 8.339156474); Conc (Species "Nc", 0.610521422);
   Conc (Species "sXm", 3.318621873); Conc (Species "U", 4.591068078);
   Conc (Species "pEj", 7.560675804); Conc (Species "u", 7.551139392);
   Conc (Species "PeJ", 1.299001615); Conc (Species "eTy", 4.056493752);
   Conc (Species "J", 6.796927057); Conc (Species "lQf", 8.336936571);
   Conc (Species "GVaP", 8.480286098); Conc (Species "Lq", 0.2413303766);
   Conc (Species "gVaP", 3.210938105); Conc (Species "Vk", 7.551859016);
   Conc (Species "aFuZ", 7.692152581); Conc (Species "ChWb", 7.604654269);
   Conc (Species "Xm", 1.035962443);
   Step [Comp (Mod (Mul (Species "yNsH", Species "hWb", Species "eTy")))];
   Step [Comp (Mod (Div (Species "Xm", Species "GVaP", Species "Q")))];
   Step [Comp (Mod (Sub (Species "wIxC", Species "gVaP", Species "pEj")))];
   Step [Comp (Mod (Sqrt (Species "Ty", Species "ChWb")))];
   Step [Comp (Mod (Mul (Species "Xm", Species "hWb", Species "Ty")))];
   Step [Comp (Mod (Add (Species "ZoTi", Species "wIxC", Species "VuZ")))];
   Step [Comp (Mod (Mul (Species "VaP", Species "Di", Species "aFuZ")))];
   Step [Comp (Mod (Sub (Species "VaP", Species "Q", Species "c")))];
   Step [Comp (Mod (Div (Species "c", Species "m", Species "oDiX")))];
   Step [Comp (Mod (Sqrt (Species "Ty", Species "ChWb")))];
   Step [Comp (Mod (Ld (Species "ZoTi", Species "wIxC")))];
   Step [Comp (Mod (Sub (Species "VuZ", Species "eJ", Species "Bg")))];
   Step [Comp (Mod (Ld (Species "c", Species "k")))]]

// simPlot 1.0 0.04 400.0 failing ["GVaP";"Q";"Xm";"X3";"X21"; "c"; "VaP";"SubHelper0"]

// Check.Quick (validate 0.25)

printfn "%O" (parse "crn={};")
// printfn "%A" (examples |> List.map parse |> List.map (fun (CRN roots) -> roots) |> List.map validArgsProp)