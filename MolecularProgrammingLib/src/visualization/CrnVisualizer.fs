(*
    Date: XX.06.2023
    Author: 
*)
module CrnVisualizer

open Plotly.NET
open CrnTypes

let updateMapFromKV stepped x map k v =
    match Map.tryFind k map with
    | Some(vn) ->
        let (x', y') = List.last vn
        let points = if stepped then [ (x, y'); (x, v) ] else [ (x, v) ]
        Map.add k (vn @ points) map
    | None -> Map.add k ((if x <> 0.0 then [ (0, 0.0); (x, 0.0) ] else []) @ [ (x, v) ]) map

let updateMapFromState stepped speciesInclude map (PosState(env, n, _)) =
    Map.fold (updateMapFromKV stepped n) map (Map.filter (fun species _ -> (List.contains species speciesInclude)) env)

let plotSpecies (Species(name), points) =
    let (xs, ys) = List.unzip points

    Chart.Line(xs, ys)
    |> Chart.withTraceInfo (Name = name)
    |> Chart.withLineStyle (Width = 2.0, Dash = StyleParam.DrawingStyle.Solid)

let visualize stepped (speciesNames: string list) (states: PositionedStateType seq) =
    let species = List.map Species speciesNames
    let plotMap = Seq.fold (updateMapFromState stepped species) Map.empty states
    let sortedSpecies = List.map (fun sp -> (sp, (Map.find sp plotMap))) species
    sortedSpecies |> List.rev |> List.map plotSpecies |> Chart.combine |> Chart.show
