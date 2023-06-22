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
open CrnGenerator
open CrnCompiler
open CrnTypeChecker
open CrnProperties
open CrnDrawTree
open FsCheck

printfn "Molecular Programming Library\n"

// Choose example CRN
let randIndex = System.Random().Next(examples.Length)
let example = examples.[randIndex]
printfn "Example CRN:\n%s\n" example

// Parse a CRN from a string
let exampleCrn = parse example
printfn "Parsed CRN: %s\n" (crnToString exampleCrn)

// Draw abstract syntax tree
let exampleSyntaxTree = crnToTree exampleCrn
printfn "Syntax tree:\n%O\n" exampleSyntaxTree

// Check that CRN is well-formed and that the dependency order property holds
assert (isWellFormedCrn false exampleCrn)
assert (dependencyOrderProp exampleCrn)

// Visualize CRN concentrations
let states = interpreter exampleCrn

let speciesNames =
    State.getAllSpecies (Seq.head states) |> List.map (fun (Species name) -> name)

printfn "Plotting interpreter output...\n"
interPlot 200 exampleCrn speciesNames

// Compile CRN to reactions
let reactions, concs = compile 1.0 exampleCrn
reactionsPrettyPrint reactions

// Simulate reactions and plot results
printfn "\nPlotting simulator output...\n"
simPlot 1.0 0.01 10 exampleCrn speciesNames

// Validate compiled and interpreted sequences
validate 0.1 exampleCrn

// Property-based testing on generated CRNs
CrnGenerator.initialize ()

let _ = Check.Quick isWellFormedCrn
let _ = Check.Quick dependencyOrderProp
