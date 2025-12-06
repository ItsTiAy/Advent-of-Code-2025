module Part1

let run (input: string seq) =
    let values, operation =
        input
        |> Seq.map (fun x -> x.Split(' ', System.StringSplitOptions.RemoveEmptyEntries))
        |> Seq.toArray
        |> fun arr -> Array.take (arr.Length - 1) arr |> Array.map (Array.map int64), Array.last arr

    let operator =
        function
        | "*" -> (*)
        | "+" -> (+)
        | _ -> (+)

    let calculate op index =
        values[1..]
        |> Array.fold (fun acc row -> operator op acc row[index]) (values[0][index])

    operation |> Array.mapi (fun index op -> calculate op index) |> Array.sum
