// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO
open System.Text.RegularExpressions

let readLines (filePath: String) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let mergePassportLines input = 
    let reduce result value =
        match (result, value) with
        | (a::tail,"") -> ""::a::tail
        | (""::tail, v) -> v::tail
        | (a::tail, v) -> (a+" "+v)::tail
    input |> Seq.fold reduce [""] |> Seq.toArray     

let toMap (line: String) =
    let arrayToPair (a:String[]) = (a.[0],a.[1])
    let toPair (s:String) = s.Split ":" |> Seq.toArray |> arrayToPair
    line.Split " " |> Seq.map(toPair) |> Map.ofSeq 

let mandatory_fields:String[] = [|"byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"|] 

let hasRequiredFields (passport:Map<String,String>) =
    let isMissing x = passport.ContainsKey x |> not 
    let missing = mandatory_fields |> Seq.filter isMissing |> Seq.toArray 
    missing |> Seq.isEmpty 

let byrRE = new Regex("^(19[2-9][0-9]|200[0-2])$");
let iyrRE = new Regex("^20(1[0-9]|20)$");
let eyrRE = new Regex("^20(2[0-9]|30)$");
let hgtRE = new Regex("^(1([5-8][0-9]|9[0-3])cm|(59|6[0-9]|7[0-6])in)$");
let hclRE = new Regex("^#[0-9a-f]{6}$");
let eclRE = new Regex("^(amb|blu|brn|gry|grn|oth|hzl)");
let pidRE = new Regex("^[0-9]{9}$");

let validations = [("byr",byrRE);("iyr",iyrRE);("eyr",eyrRE)
                   ("hgt",hgtRE);("hcl",hclRE);("ecl",eclRE);("pid",pidRE)]

let isValid (passport:Map<String,String>) field (regex:Regex) =
    let value = passport.[field]
    let ok = regex.IsMatch(value)
    // printfn "Validate %A %A => %A" regex value ok 
    ok
    
let allFieldsValid (passport:Map<String,String>) =
   let isInvalid (field, re) = isValid passport field re |> not 
   validations |> Seq.filter isInvalid |> Seq.isEmpty  
    
[<EntryPoint>]
let main argv =
    let pws =  "/Users/xeno/projects/aoc2020/day4_fs/input.txt"
    let passports = pws |> readLines |> mergePassportLines |> Seq.map toMap |> Seq.toArray 
    printfn "Passwords#: %d" passports.Length 
    let complete = passports |> Seq.filter hasRequiredFields |> Seq.toArray
    let answer1 = complete.Length
    printfn "Answer 1: %d" answer1
    let valid = complete |> Seq.filter allFieldsValid |> Seq.toArray
    let answer2 = valid.Length 
    printfn "Answer 2: %d" answer2
    0