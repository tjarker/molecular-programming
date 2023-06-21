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

Check.Quick (validate 0.25)

let failing = CRN [Conc (Species "aH", 1.699490845); Conc (Species "Xc", 1.740912397);
   Conc (Species "Ob", 2.499320048); Conc (Species "bDD", 4.254531281);
   Conc (Species "nDQ", 7.268030183); Conc (Species "U", 9.853852977);
   Conc (Species "HaiN", 3.63440204); Conc (Species "KbU", 6.46520191);
   Conc (Species "lUx", 0.5089660345); Conc (Species "iNcH", 1.437400814);
   Conc (Species "D", 2.695950497); Conc (Species "fUz", 6.110783675);
   Conc (Species "k", 9.483624058); Conc (Species "FuZ", 9.49048188);
   Conc (Species "k", 2.308705334); Conc (Species "ZeT", 5.470515181);
   Conc (Species "uJo", 8.59524587); Conc (Species "W", 6.694084604);
   Conc (Species "Bg", 6.075830584); Conc (Species "wLqF", 1.139410736);
   Conc (Species "Bg", 6.265143685); Conc (Species "qVkP", 9.446865381);
   Conc (Species "La", 9.571021408); Conc (Species "nChW", 9.452950511);
   Conc (Species "sXmR", 2.026808788); Conc (Species "Nc", 5.359855177);
   Conc (Species "sXm", 8.753448761); Conc (Species "H", 6.501197831);
   Conc (Species "cRd", 1.604481555); Conc (Species "E", 4.786203251);
   Conc (Species "JoD", 6.26800643); Conc (Species "eTy", 9.682839608);
   Conc (Species "J", 9.66735748); Conc (Species "yDs", 2.456101066);
   Conc (Species "TfUz", 5.657278889); Conc (Species "vK", 8.819317618);
   Conc (Species "aFuZ", 8.882107633); Conc (Species "Vk", 6.324848462);
   Conc (Species "aZeT", 1.296633491); Conc (Species "PuJo", 4.711466669);
   Conc (Species "rW", 9.613946122); Conc (Species "MbgV", 9.824969389);
   Conc (Species "R", 0.996845641); Conc (Species "mBg", 2.226024338);
   Conc (Species "B", 5.598864721);
   Step [Comp (Mod (Add (Species "J", Species "vK", Species "Vk")))];
   Step [Comp (Mod (Mul (Species "Bg", Species "aFuZ", Species "cRd")))];
   Step [Comp (Mod (Div (Species "yDs", Species "HaiN", Species "U")))];
   Step [Comp (Mod (Cmp (Species "bDD", Species "rW")))];
   Step [Comp (Mod (Add (Species "La", Species "La", Species "k")))];
   Step [Comp (Mod (Div (Species "uJo", Species "uJo", Species "lUx")))];
   Step [Comp (Mod (Mul (Species "Vk", Species "k", Species "nDQ")))];
   Step [Comp (Mod (Div (Species "ZeT", Species "ZeT", Species "KbU")))];
   Step [Cond (IfGT [Mod (Ld (Species "lUx", Species "aH"))])];
   Step [Comp (Mod (Sub (Species "cRd", Species "cRd", Species "Bg")))];
   Step [Comp (Mod (Sub (Species "rW", Species "rW", Species "J")))];
   Step [Comp (Mod (Sub (Species "uJo", Species "uJo", Species "lUx")))];
   Step [Comp (Mod (Cmp (Species "PuJo", Species "eTy")))];
   Step [Cond (IfLE [Mod (Sub (Species "mBg", Species "Bg", Species "D"))])];
   Step [Comp (Mod (Cmp (Species "Nc", Species "W")))]]

// simPlot 0.8 0.04 120.0 failing ["BqV";"CrW";"g";"SubHelper0";"X6"]
