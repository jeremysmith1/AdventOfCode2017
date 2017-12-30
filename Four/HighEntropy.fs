module HighEntropy
open System.Collections.Generic
open checksum
open System
open System

let lines = System.IO.File.ReadLines(@".\Four\input.txt")

let readmatrix =
    Seq.map ((fun (line: string) -> line.Split ' ') >> Seq.toList) lines |> Seq.toList



let containsNoDups (passphrase: list<string>) =
    let dict = new Dictionary<string, bool>()

    let rec isValid remainingList (collection: Dictionary<string,bool>) =
        match remainingList with
        | [] -> true
        | x::xs ->  if collection.ContainsKey x
                    then false
                    else collection.Add(x, true) |> ignore
                         isValid xs collection

    isValid passphrase dict


let rec areAnagram (x:string) (t:string) =
    if x.Length  <> t.Length 
    then false else
    if x.Length = 0
    then true else 
    let reply = x.[0].ToString ()
    areAnagram
        (x.Replace (reply,""))
        (t.Replace (reply,""))

let rec containsNoAnnograms (passphrase: list<string>) =
    match passphrase with
    | [] -> true
    | x::xs -> if (List.forall (fun t -> not (areAnagram t x)) xs) 
                then containsNoAnnograms xs 
                else false

let rec GoThroughSeq remainingList numOfValid =
    match remainingList with
    | [] -> numOfValid
    | x::xs -> GoThroughSeq xs (if (containsNoDups x && containsNoAnnograms x)
                                then numOfValid + 1 
                                else numOfValid)

let answer1 = GoThroughSeq readmatrix 0
