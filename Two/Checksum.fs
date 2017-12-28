module checksum

let lines = System.IO.File.ReadLines(@".\Two\input.txt")

let readmatrix =
    Seq.map ((fun (line: string) -> line.Split '\t') >> (Seq.map int) >> Seq.toList) lines |> Seq.toList

let AnswerOne = 
    let rec getChecksum (rest: list<list<int>>) acc =
        match rest with
        | [] -> acc
        | x::xs -> 
            let max, min = (Seq.max x, Seq.min x)
            let value = max - min
            getChecksum xs (acc + value)
    
    getChecksum readmatrix 0

let AnswerTwo =
    let rec checkSumValue (fullList: list<int>) (index: int) =
        let result = List.tryFind (fun x -> (fullList.[index] <> x && x % fullList.[index]=0)) fullList
        match result with
        | Some x -> (x / fullList.[index])
        | None -> checkSumValue fullList (index + 1)

    let rec getChecksum (rest: list<list<int>>) (acc: int) =
        match rest with
        | [] -> acc
        | x::xs ->
            getChecksum xs ((checkSumValue x 0) + acc)

    getChecksum readmatrix 0