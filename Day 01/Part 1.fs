module Part1

let run (input: string seq) =
    let range = 100

    let step (dial, password) line =

        if System.String.IsNullOrWhiteSpace line then
            dial, password
        else
            let amount = line.[1..] |> int

            let dir =
                match line.[0] with
                | 'L' -> -1
                | 'R' -> 1
                | _ -> 0

            let change = (amount + range) % range * dir
            let raw = dial + change

            let updatedDial = (raw + range) % range
            let updatedPassword = if updatedDial = 0 then password + 1 else password

            updatedDial, updatedPassword

    let _, finalPassword = Seq.fold step (50, 0) input

    finalPassword
