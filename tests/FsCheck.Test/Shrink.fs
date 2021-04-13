namespace FsCheck.Test

module ShrinkTest =

    open Xunit
    open FsCheck
    open FsCheck.Internals

    [<Fact>]
    let ``should bind``() =
        let s1 i = Shrink.ofShrinker (fun a -> if a > 0 then Seq.singleton (a-1) else Seq.empty) id i
        let s2 i = Shrink.ofShrinker (fun a -> if a < 0 then Seq.singleton (a+1) else Seq.empty) id i
        let sample1 = Shrink.sample (s1 2)
        let sample2 = Shrink.sample (s2 -2)
        let bind = s1 2 |> Shrink.bind (fun i1 -> s2 (-i1) |> Shrink.map (fun i2 -> (i1, i2)))
        let r = Shrink.sample bind
        let b = 3
        ()
