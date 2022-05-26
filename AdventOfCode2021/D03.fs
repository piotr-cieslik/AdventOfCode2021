module D03

open System

let input = System.IO.File.ReadAllLines("D03.txt")

let part1 () =
    let numberOfBits = input[0].Length
    let initial = Array.replicate numberOfBits 0
    
    let sumOfBits =
        input
        |> Array.fold (fun state value ->
                Seq.toArray value
                |> Array.map (fun x -> string x)
                |> Array.map (fun x -> int x)
                |> Array.zip state
                |> Array.map (fun (x, y) -> x + y)
            ) initial

    printfn "%A" sumOfBits
    let treshhold = input.Length / 2
    let gammaBits =
        sumOfBits
        |> Array.map (fun x -> if x > treshhold then 1 else 0)
        |> Array.map (fun x -> x.ToString())
        |> String.concat ""
    let gammaBits32 = gammaBits.PadLeft ((32 - numberOfBits), '0')
    let gamma = Convert.ToInt32(gammaBits32, 2)
    printfn "gamma: %i" gamma

    let epsilonBits =
        sumOfBits
        |> Array.map (fun x -> if x > treshhold then 0 else 1)
        |> Array.map (fun x -> x.ToString())
        |> String.concat ""
    let epsilonBits32 = epsilonBits.PadLeft ((32 - numberOfBits), '0')
    let epsilon = Convert.ToInt32(epsilonBits32, 2)
    printfn "epsilon: %i" epsilon

    gamma * epsilon

let part2 () =
    0