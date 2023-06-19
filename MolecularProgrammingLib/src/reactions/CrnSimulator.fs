(*
    Date: XX.06.2023
    Author: 
*)
module CrnSimulator

open CrnTypes
open CrnInterpreter

let dt = 0.001

let rec count y acc =
    function
    | [] -> acc
    | x :: xs -> count y (if y = x then acc + 1 else acc) xs

let netChange sp ((r, p, _): Reaction) =
    let leftCount = count sp 0 r
    let rightCount = count sp 0 p
    rightCount - leftCount

let concChange ((r, p, k) as (rxn: Reaction)) constProd sp =
    (float k) * (netChange sp rxn |> float) * constProd

let simRxn state changeMap ((r, p, n) as (rxn: Reaction)) =
    let reactConcs = List.map (fun sp -> State.get sp state) r
    let ms = r |> Seq.countBy id |> Map.ofSeq

    let constProd =
        List.zip r reactConcs
        |> List.map (fun (sp, c) -> pown c (Map.find sp ms))
        |> List.reduce (fun x y -> x * y)

    let uniqueSpecies = (r @ p) |> Set.ofList |> Set.toList

    List.fold
        (fun changeMap sp ->
            let change = concChange rxn constProd sp

            match Map.tryFind sp changeMap with
            | Some(value) -> Map.add sp (value + change) changeMap
            | None -> Map.add sp change changeMap)
        changeMap
        uniqueSpecies


let nextState rxns state =

    let changeMap = List.fold (simRxn state) Map.empty rxns

    Map.fold (fun s' sp dif -> State.update sp ((State.get sp state) + (dif * dt)) s') state changeMap
    |> State.tick

let simulator (rxns: Reaction list, concs: Map<Species, float>) =

    State(concs, 0, (false, false))
    |> Seq.unfold (fun state -> Some(state, state |> nextState rxns))
    |> Seq.cache
