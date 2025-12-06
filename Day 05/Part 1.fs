module Part1

let run (input: string seq) =
    let ranges, inputs =
        let index = input |> Seq.findIndex (fun empty -> empty = "")
        let array = input |> Seq.toArray

        array.[0 .. index - 1]
        |> Array.map (fun x ->
            let range = x.Split '-'
            int64 range[0], int64 range[1]),
        array.[index + 1 ..] |> Array.map int64

    let checkFresh id =
        ranges
        |> Array.exists (fun (a, b) -> id >= a && id <= b)
        |> function
            | true -> 1
            | false -> 0

    inputs |> Array.sumBy checkFresh
