module CrnInterpreter

open CrnTypes

type Flag = bool * bool
type StateType = State of Map<Species, float> * int * Flag

module State =
    let init concs = State(Map concs, 0, (false, false))
    let update key value (State(env, n, flag)) = State(Map.add key value env, n, flag)

    let get key (State(env, n, flag)) =
        match Map.tryFind key env with
        | Some(value) -> value
        | None -> 0.0

    let setFlag equal greater (State(env, n, flag)) = State(env, n, (equal, greater))
    let tick (State(env, n, flag)) = State(env, n + 1, flag)
    let getAllSpecies (State(env, _, _)) = Map.keys env |> Seq.toList


let applyModule state =
    function
    | Ld(a, b) -> State.update b (State.get a state) state
    | Add(a, b, c) -> State.update c (State.get a state + State.get b state) state
    | Sub(a, b, c) -> State.update c (State.get a state - State.get b state) state
    | Mul(a, b, c) -> State.update c (State.get a state * State.get b state) state
    | Div(a, b, c) -> State.update c (State.get a state / State.get b state) state
    | Sqrt(a, b) -> State.update b (State.get a state |> sqrt) state
    | Cmp(a, b) ->
        let va = State.get a state
        let vb = State.get b state
        State.setFlag (va = vb) (va > vb) state

let applyComputation state =
    function
    | Mod(m) -> applyModule state m
    | Rxn(_, _, _) -> failwith "Reactions are not supported by the interpreter"


let applyConditional (State(_, _, (eq, gt)) as state) =
    function
    | IfGT(comps) -> if gt then List.fold applyComputation state comps else state
    | IfGE(comps) ->
        if gt || eq then
            List.fold applyComputation state comps
        else
            state
    | IfEQ(comps) -> if eq then List.fold applyComputation state comps else state
    | IfLT(comps) ->
        if (not gt && not eq) then
            List.fold applyComputation state comps
        else
            state
    | IfLE(comps) ->
        if (not gt || eq) then
            List.fold applyComputation state comps
        else
            state

let applyCommand state =
    function
    | Comp(c) -> applyComputation state c
    | Cond(c) -> applyConditional state c


let apply state =
    function
    | Conc(_, _) -> failwith "Exptected a Step but received a Conc"
    | Step(cmds) -> List.fold applyCommand state cmds

let interpreter (CRN roots) =
    let state =
        roots
        |> List.choose (function
            | (Conc(s, c)) -> Some(s, c)
            | _ -> None)
        |> State.init

    let steps =
        roots
        |> List.choose (function
            | (Step cmds) -> Some(Step cmds)
            | _ -> None)

    let stepCount = List.length steps

    if stepCount = 0 then
        Seq.singleton state
    else
        state
        |> Seq.unfold (fun (State(env, n, flags) as state) ->
            let stepIndex = n % stepCount
            let step = steps.[stepIndex]
            let newState = apply state step |> State.tick
            Some(state, newState))
