module CrnVisualizer

open Plotly.NET
open CrnTypes
open CrnInterpreter

let updateMapFromKV x map k v =
    match Map.tryFind k map with
    | Some(vn) ->
        let (_, y') = List.last vn
        Map.add k (vn @ [ (x, y'); (x, v) ]) map
    | None -> Map.add k [ (0, 0.0); (x, 0.0); (x, v) ] map

let updateMapFromState speciesInclude map (State(env, n, _)) =
    Map.fold (updateMapFromKV n) map (Map.filter (fun species _ -> (List.contains species speciesInclude)) env)

let plotSpecies (Species(name), points) =
    let (xs, ys) = List.unzip points

    Chart.Line(xs, ys)
    |> Chart.withTraceInfo (Name = name)
    |> Chart.withLineStyle (Width = 2.0, Dash = StyleParam.DrawingStyle.Solid)

let visualize (species: Species list) (states: StateType seq) =
    let plotMap = Seq.fold (updateMapFromState species) Map.empty states
    let sortedSpecies = List.map (fun sp -> (sp, (Map.find sp plotMap))) species
    sortedSpecies |> List.rev |> List.map plotSpecies |> Chart.combine |> Chart.show
