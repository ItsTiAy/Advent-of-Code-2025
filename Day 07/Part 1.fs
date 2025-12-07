module Part1

let run (input: string seq) =
    let start = input |> Seq.head |> Seq.findIndex (fun a -> a = 'S')

    let total =
        input
        |> Seq.tail
        |> Seq.map Seq.toArray
        |> Seq.fold
            (fun (currentSplits, splitCount) row ->
                let splitters =
                    row
                    |> Array.indexed
                    |> Array.choose (fun (index, value) -> if value = '^' then Some index else None)

                let hits =
                    currentSplits
                    |> Array.filter (fun splitIndex -> Array.contains splitIndex splitters)
                    |> Array.length

                currentSplits
                |> Array.collect (fun splitIndex ->
                    if splitters |> Array.contains splitIndex then
                        [| splitIndex - 1; splitIndex + 1 |]
                    else
                        [| splitIndex |])
                |> Array.distinct,
                splitCount + hits)
            ([| start |], 0)

    snd total
