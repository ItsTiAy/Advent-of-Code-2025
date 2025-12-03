module Part1

let run (input: string seq) =
    let checkBattery (battery: string) =
        battery
        |> Seq.map (fun c -> int c - int '0')
        |> Seq.mapi (fun i x -> i, x)
        |> Seq.fold
            (fun (a, b) (i, x) ->
                if x > a && i <> battery.Length - 1 then x, 0
                elif x > b then a, x
                else a, b)
            (0, 0)
        |> fun (a, b) -> [ a; b ]
        |> List.map string
        |> String.concat ""
        |> int

    let total = input |> Seq.sumBy (string >> checkBattery)
    total
