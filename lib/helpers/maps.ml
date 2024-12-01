open Core

let map_value map element = Map.find map element |> Option.value ~default:0
