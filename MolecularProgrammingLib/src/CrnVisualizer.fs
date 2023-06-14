module CrnVisualizer

open Plotly.NET
open CrnTypes
open CrnInterpreter

let updateMapFromKVN n map k (v: float) =
    match Map.tryFind k map with
    | Some(vn) ->
        let (_, y') = List.last vn
        Map.add k (vn @ [ (n, y'); (n, v) ]) map
    | None -> Map.add k [ (0, 0.0); (n, 0.0); (n, v) ] map
// maybe TODO: Check for existing keys that are not updated in a state

let updateMapFromState speciesInclude map (State(env, n, _)) =
    Map.fold (updateMapFromKVN n) map (Map.filter (fun species _ -> (List.contains species speciesInclude)) env)

let plotSpecies (Species(name), points) =
    let (xs, ys) = List.unzip points

    Chart.Line(xs, ys)
    |> Chart.withTraceInfo (Name = name)
    |> Chart.withLineStyle (Width = 2.0, Dash = StyleParam.DrawingStyle.Solid)

let visualize (species: Species list) (states: StateType seq) =
    let plotMap = Seq.fold (updateMapFromState species) Map.empty states
    let sortedSpecies = List.map (fun sp -> (sp, (Map.find sp plotMap))) species
    sortedSpecies |> List.rev |> List.map plotSpecies |> Chart.combine |> Chart.show
