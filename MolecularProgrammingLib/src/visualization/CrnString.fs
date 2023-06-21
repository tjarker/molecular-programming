(*
    Date: XX.06.2023
    Author: 
*)
module CrnString

open CrnTypes

let moduleToString =
    function
    | Ld(Species sp1, Species sp2) -> $"ld[{sp1},{sp2}]"
    | Add(Species sp1, Species sp2, Species sp3) -> $"add[{sp1},{sp2},{sp3}]"
    | Sub(Species sp1, Species sp2, Species sp3) -> $"sub[{sp1},{sp2},{sp3}]"
    | Mul(Species sp1, Species sp2, Species sp3) -> $"mul[{sp1},{sp2},{sp3}]"
    | Div(Species sp1, Species sp2, Species sp3) -> $"div[{sp1},{sp2},{sp3}]"
    | Sqrt(Species sp1, Species sp2) -> $"sqrt[{sp1},{sp2}]"
    | Cmp(Species sp1, Species sp2) -> $"cmp[{sp1},{sp2}]"

let speciesListToString xs =
    xs |> List.map (fun (Species name) -> name) |> String.concat "+ "

let computationToString =
    function
    | Mod(modu) -> moduleToString modu
    | Rxn(sps1, sps2, n) ->
        let lhs = speciesListToString sps1
        let rhs = speciesListToString sps2
        $"rxn[{lhs},{rhs},{n}]"

let conditionalToString =
    function
    | IfGT xs -> "ifGT[{" + (List.map computationToString xs |> String.concat ", ") + "}]"
    | IfGE xs -> "ifGE[{" + (List.map computationToString xs |> String.concat ", ") + "}]"
    | IfEQ xs -> "ifEQ[{" + (List.map computationToString xs |> String.concat ", ") + "}]"
    | IfLT xs -> "ifLT[{" + (List.map computationToString xs |> String.concat ", ") + "}]"
    | IfLE xs -> "ifLE[{" + (List.map computationToString xs |> String.concat ", ") + "}]"

let commandToString =
    function
    | Comp comp -> computationToString comp
    | Cond cond -> conditionalToString cond

let rootToString =
    function
    | Conc(Species sp, c) -> sprintf "conc[%s, %.0G]" sp c
    | Step cs -> cs |> List.map commandToString |> String.concat ", " |> sprintf "step[{%s}]"

let crnToString (CRN roots) =
    let rs = roots |> List.map rootToString |> String.concat ", "
    $"crn = {{{rs}}};"

let removeWhiteSpace str =
    str
    |> String.collect (fun c ->
        if List.contains c [ ' '; '\n'; '\t'; '\r' ] then
            ""
        else
            string c)
