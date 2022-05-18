module D02

let input = System.IO.File.ReadAllLines("D02P1.txt")

type Change = {Direction: string; Value: int}
type Position = {X: int; Y: int}

let part1 () =
    let initialPosition = {X=0; Y=0}
    input
    |> Array.map (fun change -> change.Split(" "))
    |> Array.map (fun change -> { Direction = change[0]; Value = int change[1] }) // Split to direction and value
    |> Array.fold (fun position change ->
        match change.Direction with
            | "forward" -> {X=position.X + change.Value; Y=position.Y}
            | "down" -> {X=position.X; Y=position.Y + change.Value}
            | "up" -> {X=position.X; Y=position.Y - change.Value}
            | _ -> {X=position.X; Y=position.Y}) initialPosition
    |> (fun position -> position.X * position.Y)