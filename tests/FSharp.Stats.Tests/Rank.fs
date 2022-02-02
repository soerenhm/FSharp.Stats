module RankTests
open Expecto
open System
open FSharp.Stats
    

[<Tests>]
let rankTests = 
    let arrayA = [|6.;6.;6.;1.;-2.;3.;3.;3.;3.;2.5;4.;-2.;0.;5.;1.|]
    let arrayB = [|6;6;6;1;-2;3;3;3;3;2;4;-2;0;5;1|]
    let arrayC = [|-infinity;nan;nan;1.;-2.;3.;3.;3.;infinity;2.5;infinity;-2.;0.;5.;1.|]

    let exAvgA = [|14.0;14.0;14.0;4.5;1.5;8.5;8.5;8.5;8.5;6.0;11.0;1.5;3.0;12.0;4.5|]
    let exFstA = [|13.0;14.0;15.0;4.0;1.0;7.0;8.0;9.0;10.0;6.0;11.0;2.0;3.0;12.0;5.0|]
    let exMaxA = [|15.0;15.0;15.0;5.0;2.0;10.0;10.0;10.0;10.0;6.0;11.0;2.0;3.0;12.0;5.0|]
    let exMinA = [|13.0;13.0;13.0;4.0;1.0;7.0;7.0;7.0;7.0;6.0;11.0;1.0;3.0;12.0;4.0|]

    let exAvgC = [|1.0;14.0;15.0;5.5;2.5;9.0;9.0;9.0;12.0;7.0;13.0;2.5;4.0;11.0;5.5|]
    let exFstC = [|1.0;14.0;15.0;5.0;2.0;8.0;9.0;10.0;12.0;7.0;13.0;3.0;4.0;11.0;6.0|]
    let exMaxC = [|1.0;14.0;15.0;6.0;3.0;10.0;10.0;10.0;12.0;7.0;13.0;3.0;4.0;11.0;6.0|]
    let exMinC = [|1.0;14.0;15.0;5.0;2.0;8.0;8.0;8.0;12.0;7.0;13.0;2.0;4.0;11.0;5.0;|]

    testList "Rank" [
        testCase "rankAverage" <| fun () -> 
            Expect.sequenceEqual (Rank.rankAverage arrayA) exAvgA "ranks should be equal"
        testCase "rankFirst" <| fun () -> 
            Expect.sequenceEqual (Rank.rankFirst arrayA) exFstA "ranks should be equal"
        testCase "rankMax" <| fun () -> 
            Expect.sequenceEqual (Rank.rankMax arrayA) exMaxA "ranks should be equal"
        testCase "rankMin" <| fun () -> 
            Expect.sequenceEqual (Rank.rankMin arrayA) exMinA "ranks should be equal"
        testCase "rankFirstInt" <| fun () -> 
            Expect.sequenceEqual (Rank.rankFirst arrayB) exFstA "ranks for ints should be equal"


        testCase "rankAverageNaNLast" <| fun () -> 
            Expect.sequenceEqual (Rank.rankAverageNaNLast arrayC) exAvgC "ranks should be equal"
        testCase "rankFirstNaNLast" <| fun () -> 
            Expect.sequenceEqual (Rank.rankFirstNaNLast arrayC) exFstC "ranks should be equal"
        testCase "rankMaxNaNLast" <| fun () -> 
            Expect.sequenceEqual (Rank.rankMaxNaNLast arrayC) exMaxC "ranks should be equal"
        testCase "rankMinNaNLast" <| fun () -> 
            Expect.sequenceEqual (Rank.rankMinNaNLast arrayC) exMinC "ranks should be equal"

    ]

