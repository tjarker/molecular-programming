(*
    Date: XX.06.2023
    Author: 
*)
module CrnTypes

type Species = Species of string

type Module =
    | Ld of Species * Species
    | Add of Species * Species * Species
    | Sub of Species * Species * Species
    | Mul of Species * Species * Species
    | Div of Species * Species * Species
    | Sqrt of Species * Species
    | Cmp of Species * Species

type Reaction = Species list * Species list * float

type Computation =
    | Mod of Module
    | Rxn of Reaction

type Conditional =
    | IfGT of Computation list
    | IfGE of Computation list
    | IfEQ of Computation list
    | IfLT of Computation list
    | IfLE of Computation list

type Command =
    | Comp of Computation
    | Cond of Conditional

type Root =
    | Conc of Species * float
    | Step of Command list

type CRN = CRN of Root list

type Flag = bool * bool
type StateType = State of Map<Species, float> * int * Flag
type PositionedStateType = PosState of Map<Species, float> * float * Flag

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


    let pretty (State(env, n, flag)) =
        let mapStr =
            env
            |> Map.toList
            |> List.map (fun (Species sp, conc) -> $"{sp} = {conc}")
            |> String.concat ", "

        sprintf $"State {n}:\n\t{mapStr}\n\t{flag}"

    let prettyPrint state = printfn "%s" (pretty state)
