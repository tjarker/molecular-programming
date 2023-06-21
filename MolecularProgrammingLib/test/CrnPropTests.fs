(*
    Date: 15.06.2023
    Author: Steffan
*)

module CrnPropTests

open CrnTypes
open CrnGenerator
open CrnParser
open CrnString
open CrnProperties
open FsCheck.Xunit

type TestFixture() =
    do CrnGenerator.initialize () |> ignore

    [<Property>]
    member __.``Parsed CRN is equal to original``(crn: CRN) = crn = (crn |> crnToString |> parse)
    
    [<Property>]
    member __.``Output from CRN step does not depend on command order``(crn: CRN) =
        orderDoesntMatterProp 10 crn