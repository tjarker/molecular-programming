module CrnUnitTests

open CrnExamples
open CrnParser
open CrnString
open CrnTypeChecker
open CrnProperties
open Xunit

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
