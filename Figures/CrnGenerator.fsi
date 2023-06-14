module CrnGenerator

open FsCheck
open CrnTypes

val crnGen: Gen<CRN>

[<Class>]
type CrnGenerator =

    static member CRN: unit -> FsCheck.Arbitrary<CRN>

    static member initialize: unit -> FsCheck.TypeClass.TypeClassComparison
