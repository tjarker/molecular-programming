module CrnUnitTests

open CrnExamples
open CrnParser
open CrnString
open Xunit

let removeWhiteSpace (s: string) =
    s.Replace(" ", "").Replace("\t", "").Replace("\r\n", "")

[<Fact>]
let ``Parsing CRN examples`` () =
    for example in examples do
        let result = example |> parse |> crnToString |> removeWhiteSpace
        let example' = removeWhiteSpace example
        Assert.Equal(example', result)
