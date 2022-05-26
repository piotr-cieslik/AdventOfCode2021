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

    let treshhold = input.Length / 2
    let gammaBits =
        sumOfBits
        |> Array.map (fun x -> if x > treshhold then 1 else 0)
        |> Array.map (fun x -> x.ToString())
        |> String.concat ""
    let gammaBits32 = gammaBits.PadLeft ((32 - numberOfBits), '0')
    let gamma = Convert.ToInt32(gammaBits32, 2)
    //printfn "gamma: %i" gamma

    let epsilonBits =
        sumOfBits
        |> Array.map (fun x -> if x > treshhold then 0 else 1)
        |> Array.map (fun x -> x.ToString())
        |> String.concat ""
    let epsilonBits32 = epsilonBits.PadLeft ((32 - numberOfBits), '0')
    let epsilon = Convert.ToInt32(epsilonBits32, 2)
    //printfn "epsilon: %i" epsilon

    gamma * epsilon

let part2 () =
    let toNumberArray (input: string[]) =
        input
        |> Array.map (fun stringValue ->
            stringValue
            |> Seq.toArray
            |> Array.map (fun x -> int (string x)))

    let toInt32 (bits: int[]) =
        let string =
            bits
            |> Array.map (fun x -> x.ToString())
            |> String.concat ""

        Convert.ToInt32(string, 2)

    let sumOfBits numberArray =
        let numberOfBits = input[0].Length
        let initial = Array.replicate numberOfBits 0
        numberArray
        |> Array.fold (fun state value ->
            value
            |> Array.zip state
            |> Array.map (fun (x, y) -> x + y))
            initial

    let mostCommonBit position numberArray =
        let sum = sumOfBits numberArray
        let treshhold = (float numberArray.Length) / 2.0
        let result = if (float sum[position]) >= treshhold then 1 else 0
        result

    let lessCommonBit position numberArray =
        mostCommonBit position numberArray
        |> (fun x -> if x = 1 then 0 else 1)

    let filterByValueOnPositoin value (position: int) (numberArray: int[][]) =
        numberArray |> Array.where (fun x -> x[position] = value)
    
    let rec oxygenGeneratorRating index numberArray =
        let bit = mostCommonBit index numberArray
        let filteredNumberArray = filterByValueOnPositoin bit index numberArray
        let formattedNumberArray = filteredNumberArray |> Array.map (fun x -> String.concat "" (x |> Array.map (fun y -> string y)))
        //printfn "Index: %i, bit: %i, filtered: %A" index bit formattedNumberArray

        match filteredNumberArray.Length with
            | 1 -> filteredNumberArray[0]
            | _ -> oxygenGeneratorRating (index + 1) filteredNumberArray

    let rec co2ScrubberRating index numberArray =
        let bit = lessCommonBit index numberArray
        let filteredNumberArray = filterByValueOnPositoin bit index numberArray
        let formattedNumberArray = filteredNumberArray |> Array.map (fun x -> String.concat "" (x |> Array.map (fun y -> string y)))
        //printfn "Index: %i, bit: %i, filtered: %A" index bit formattedNumberArray

        match filteredNumberArray.Length with
            | 1 -> filteredNumberArray[0]
            | _ -> co2ScrubberRating (index + 1) filteredNumberArray

    let numberArray = toNumberArray input
    let oxygenBits = oxygenGeneratorRating 0 numberArray
    let oxygen = toInt32 oxygenBits
    let co2Bits = co2ScrubberRating 0 numberArray
    let co2 = toInt32 co2Bits

    //printfn "oxygen: %i, co2: %i" oxygen co2
    oxygen * co2