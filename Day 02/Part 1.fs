module Part1

let run (input: string seq) =

    let valid (id: string) =
        let isValid =
            match id.Length with
            | n when n % 2 = 0 ->
                let half = n / 2

                if id[0 .. half - 1] = id[half .. n - 1] then
                    id |> int64
                else
                    0
            | _ -> 0

        isValid

    let check (idString: string) =
        let ids = Array.map int64 (idString.Split '-')
        [ ids.[0] .. ids.[1] ] |> List.sumBy (string >> valid)

    let total = input |> Seq.sumBy check

    total
