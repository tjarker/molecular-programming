(*
    Date: XX.06.2023
    Author: 
*)
module CrnInterpreter

open CrnTypes

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
        |> Seq.cache
