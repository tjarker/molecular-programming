(*
    Date: 14.06.2023
    Author: Steffan
*)
module CrnUnitTests

open CrnExamples
open CrnParser
open CrnString
open CrnTypeChecker
open Xunit
open CrnProperties

[<Fact>]
let ``Parsing CRN examples`` () =
    for example in examples do
        let result = example |> parse |> crnToString |> removeWhiteSpace
        let example' = removeWhiteSpace example
        Assert.Equal(example', result)

[<Fact>]
let ``Well-formed CRN examples`` () =
    for example in examples do
        let result = example |> parse |> isWellFormedCrn false
        Assert.True(result, "CRN is not well-formed:\n" + example)


[<Fact>]
let ``Simulator and interpreter should produce the same results`` () =
    for example in examples do
        Assert.True(validate 0.25 (example |> parse), "Simulator and interpreter disagreed for CRN:\n" + example)


[<Fact>]
let ``Dependency order CRN examples`` () =
    for example in examples do
        let result = example |> parse |> dependencyOrderProp
        Assert.True(result, "Dependency order property violated:\n" + example)
