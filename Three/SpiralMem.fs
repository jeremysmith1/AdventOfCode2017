module SpiralMem


let doMove (x,y) (dx,dy) = (x+dx, y+dy)

let solve1 n =
    let rec find loc ring tally =
        let mutable moves = []
        for i in 1 .. (2 * ring) - 1 do
            moves <- (0, 1) :: moves
        for i in 1 .. (2 * ring) do
            moves <- (-1, 0) :: moves
        for i in 1 .. (2 * ring) do
            moves <- (0, -1) :: moves
        for i in 1 .. (2 * ring) + 1 do
            moves <- (1, 0) :: moves
        let rec move loc moves tally =
            if tally = n then
                (true, loc, tally)
            else
                match moves with
                | dir :: tail -> move (doMove loc dir) tail (tally+1)
                | [] -> (false, loc, tally)
        let (found, newLoc, newTally) = move loc (List.rev moves) tally
        if found then newLoc else find newLoc (ring + 1) newTally
    find (0,0) 0 1

let findValue oldValues newValues loc =
    if Map.containsKey loc oldValues then
        Map.find loc oldValues 
    else if Map.containsKey loc newValues then
        Map.find loc newValues
    else
        0

let solve2 n =
    let rec find loc ring oldValues =
        let mutable newValues = [(0,0), 1] |> Map.ofList
        let mutable moves = []
        for i in 1 .. (2 * ring) - 1 do
            moves <- (0, 1) :: moves
        for i in 1 .. (2 * ring) do
            moves <- (-1, 0) :: moves
        for i in 1 .. (2 * ring) do
            moves <- (0, -1) :: moves
        for i in 1 .. (2 * ring) + 1 do
            moves <- (1, 0) :: moves
        let rec move loc moves =
            let mutable value = 0
            for i in -1 .. 1 do
                for j in -1 .. 1 do
                    if not (i = 0 && j = 0) then
                        value <- value + (findValue oldValues newValues (doMove loc (i,j)))
            newValues <- Map.add loc value newValues
            if value > n then
                (true, value, loc)
            else
                match moves with
                | dir :: tail -> move (doMove loc dir) tail
                | [] -> (false, value, loc)
        let (found, value, newLoc) = move loc (List.rev moves)
        if found then value else find newLoc (ring + 1) newValues
    find (1,0) 1 ([(0,0), 1] |> Map.ofList)