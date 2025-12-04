module Part1

let run (input: string seq) =
    let grid = input |> Seq.map Seq.toArray |> Seq.toArray
    let rows = grid.Length
    let cols = grid[0].Length

    let directions = [| 0, 1; 1, 0; 0, -1; -1, 0; 1, 1; 1, -1; -1, 1; -1, -1 |]

    let neighbours r c =
        directions
        |> Array.choose (fun (dr, dc) ->
            match grid[r][c] with
            | '@' ->
                let nr, nc = r + dr, c + dc

                if nr >= 0 && nr < rows && nc >= 0 && nc < cols then
                    match grid[nr][nc] with
                    | '@' -> Some 1
                    | _ -> None
                else
                    None
            | _ -> Some 4)

    seq { 0 .. rows - 1 }
    |> Seq.map (fun row ->
        seq { 0 .. cols - 1 }
        |> Seq.map (fun col -> neighbours row col |> Array.sum < 4)
        |> Seq.sumBy (fun b -> if b then 1 else 0))
    |> Seq.sum
