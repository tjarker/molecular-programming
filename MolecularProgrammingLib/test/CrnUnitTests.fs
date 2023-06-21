module CrnUnitTests

open CrnExamples
open CrnParser
open CrnString
open CrnTypeChecker
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
        let result = example |> parse |> isWellFormedCrn
        Assert.True(result)