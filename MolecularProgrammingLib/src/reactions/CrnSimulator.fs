(*
    Date: XX.06.2023
    Author: 
*)
module CrnSimulator

open CrnTypes
open CrnInterpreter

let dt = 0.04

let rec count y acc =
    function
    | [] -> acc
    | x :: xs -> count y (if y = x then acc + 1 else acc) xs

let netChange sp ((r, p, _): Reaction) =
    let leftCount = count sp 0 r
    let rightCount = count sp 0 p
    rightCount - leftCount

let concChange ((r, p, k) as (rxn: Reaction)) constProd sp =
    let res = (float k) * (netChange sp rxn |> float) * constProd

    if System.Double.IsNaN(res) then
        printfn "%f * %d * %f = %f" k (netChange sp rxn) constProd res

    res

let simRxn state changeMap ((r, p, n) as (rxn: Reaction)) =
    let reactConcs = List.map (fun sp -> State.get sp state) r
    let ms = r |> Seq.countBy id |> Map.ofSeq

    let constProd =
        List.zip r reactConcs
        |> List.map (fun (sp, c) -> pown c (Map.find sp ms))
        |> List.reduce (fun x y -> x * y)

    if System.Double.IsInfinity(constProd) then
        printfn "%A\n%A" reactConcs ms

    let uniqueSpecies = (r @ p) |> Set.ofList |> Set.toList

    List.fold
        (fun changeMap sp ->
            let change = concChange rxn constProd sp

            match Map.tryFind sp changeMap with
            | Some(value) -> Map.add sp (value + change) changeMap
            | None -> Map.add sp change changeMap)
        changeMap
        uniqueSpecies


let nextState rxns (State(_, n, _) as state) =

    let changeMap = List.fold (simRxn state) Map.empty rxns

    Map.fold
        (fun s' sp dif ->
            let was = (State.get sp state)
            let change = dif * dt
            let next = was + change

            if System.Double.IsNaN(next) then
                State.prettyPrint state
                printfn "state %d: %f + %f -> %f" n was dif next
                assert (0 = 1)

            State.update sp next s')
        state
        changeMap
    |> State.tick

let simulator (rxns: Reaction list, concs: Map<Species, float>) =
    State(concs, 0, (false, false))
    |> Seq.unfold (fun state -> Some(state, state |> nextState rxns))
    |> Seq.cache
