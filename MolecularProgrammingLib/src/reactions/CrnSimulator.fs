(*
    Date: XX.06.2023
    Author: 
*)
module CrnSimulator

open CrnTypes
open CrnInterpreter
open System.Diagnostics
open CrnVisualizer
open CrnCompiler

let resultValidater (Species sp) x state =
    Debug.Assert(not (System.Double.IsNaN(x)), $"[{sp}]/dt was NaN in {State.pretty state}")
    Debug.Assert(not (System.Double.IsInfinity(x)), $"[{sp}]/dt was Inf in {State.pretty state}")
    Debug.Assert(not (System.Double.IsNegativeInfinity(x)), $"[{sp}]/dt was -Inf in {State.pretty state}")

let rec count y acc =
    function
    | [] -> acc
    | x :: xs -> count y (if y = x then acc + 1 else acc) xs

let netChange sp ((r, p, _): Reaction) =
    let leftCount = count sp 0 r
    let rightCount = count sp 0 p
    rightCount - leftCount

let concChange ((r, p, k) as (rxn: Reaction)) (nc: int) constProd sp =
    (float k) * (float nc) * constProd

let simRxn state changeMap ((r, p, n) as (rxn: Reaction)) =
    let reactConcs = List.map (fun sp -> State.get sp state) r
    let ms = r |> Seq.countBy id |> Map.ofSeq

    let constProd =
        List.zip r reactConcs
        |> List.map (fun (sp, c) -> pown c (Map.find sp ms))
        |> List.reduce (fun x y -> x * y)

    Debug.Assert(
        not (System.Double.IsInfinity(constProd)),
        $"Reactant concentration product was Inf for {r} -> {p} in {State.pretty state}"
    )

    let uniqueSpecies = 
        List.distinct (r @ p) 
            |> List.map (fun sp -> sp, netChange sp rxn)
            |> List.filter (fun (sp,nc) -> nc <> 0)

    List.fold
        (fun changeMap (sp, nc) ->
            let change = concChange rxn nc constProd sp

            match Map.tryFind sp changeMap with
            | Some(value) -> Map.add sp (value + change) changeMap
            | None -> Map.add sp change changeMap)
        changeMap
        uniqueSpecies

let nonNegative x = if x < 0.0 then 0.0 else x

let nextState dt rxns (State(_, n, _) as state) =

    let changeMap = List.fold (simRxn state) Map.empty rxns

    Map.fold
        (fun s' sp dif ->
            let current = (State.get sp state)
            let next = current + dif * dt |> nonNegative

            resultValidater sp next state

            State.update sp next s')
        state
        changeMap
    |> State.tick

let simulator dt (rxns: Reaction list, concs: Map<Species, float>) =
    State(concs, 0, (false, false))
    |> Seq.unfold (fun state -> Some(state, state |> nextState dt rxns))
    |> Seq.cache

let takeEveryNth n s =
    Seq.unfold
        (fun state ->
            let headOpt = Seq.tryHead state
            let len = Seq.length state

            match (headOpt, len) with
            | Some(head), l when l > n -> Some(head, Seq.skip n state)
            | Some(head), _ -> Some(head, Seq.empty)
            | None, _ -> None)
        s

let simPlot speed dt time crn traces =
    let simTicks = time / dt |> int
    let pointsPerPlot = 800
    let skip = simTicks / pointsPerPlot
    let simTrace = crn |> compile speed |> simulator dt |> Seq.take simTicks

    let sparseTrace =
        if simTicks <= pointsPerPlot then
            simTrace
        else
            takeEveryNth skip simTrace

    let plotTrace =
        sparseTrace
        |> Seq.map (fun (State(env, n, flags)) -> PosState(env, (float n) * dt, flags))

    visualize false traces plotTrace
