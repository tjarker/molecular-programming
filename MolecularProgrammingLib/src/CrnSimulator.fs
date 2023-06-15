module CrnSimulator

open CrnTypes
open CrnInterpreter

let dt = 0.001

let rec count y =
    function
    | [] -> 0
    | x :: xs -> (if y = x then 1 else 0) + (count y xs)

let netChange sp ((r, p, _): Reaction) =
    let leftCount = count sp r
    let rightCount = count sp p
    rightCount - leftCount

let rxnConcChange sp ((r, p, n) as (rxn: Reaction)) state =
    let reactConcs = List.map (fun sp -> State.get sp state) r
    let ms = List.map (fun sp -> count sp r) r

    (float n)
    * (netChange sp rxn |> float)
    * (List.zip reactConcs ms
       |> List.map (fun (c, m) -> pown c m)
       |> List.reduce (fun x y -> x * y))

let concChange sp rxns state =
    List.fold (fun change rxn -> change + (rxnConcChange sp rxn state)) 0.0 rxns


let nextState rxns state =
    let sps = State.getAllSpecies state

    let changeMap = sps |> List.map (fun sp -> (sp, concChange sp rxns state))

    List.fold (fun s' (sp, dif) -> State.update sp ((State.get sp state) + (dif * dt)) s') state changeMap
    |> State.tick


let simulator (concs: list<string * float>) (rxns: Reaction list) =

    let state =
        State(concs |> List.map (fun (n, c) -> (Species n, c)) |> Map.ofList, 0, (false, false))

    state
    |> Seq.unfold (fun (State(env, n, flags) as state) -> Some(state, state |> nextState rxns))
