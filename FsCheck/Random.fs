#light

namespace FsCheck

module Random

open System

//from Hugs' Haskell implementation 
type StdGen = StdGen of int * int
                with override x.ToString() = match x with (StdGen (s1,s2)) -> sprintf "%A" (s1,s2) 

//Haskell has mod,quot, en divMod. .NET has DivRem, % and /.
// Haskell              | F#
//---------------------------------
// rem                  | %
// mod                  | ?
// quot                 | /
// div                  | ?
// divMod               | ?
// quotRem              | divRem

//since the implementation uses divMod and mod, we need to reimplement these.
//fortunately that's fairly easy given Math.DivRem
let divMod (n:int) d = 
    let (q,r) as qr = Math.DivRem(n,d) //neat F# feature here: out parameters are converted to tuples!
    if (Math.Sign(r) = -Math.Sign(d)) then (q-1,r+d) else (q,r)

let hMod n d = 
    let _,r = divMod n d
    r

let rec mkStdGen s = match s with
                     | s when s < 0 -> mkStdGen (-s)
                     | _ -> let (q, s1) = divMod s 2147483562
                            let s2 = hMod q 2147483398
                            StdGen (s1+1,s2+1)

let stdNext (StdGen (s1,s2)) =
    let k = s1 / 53668
    let s1' = 40014 * (s1 - k * 53668) - k * 12211
    let s1'' = if (s1' < 0) then s1 + 2147483563 else s1'
    let k' = s2 / 52774
    let s2' = 40692 * (s2 - k' * 52774) - k' * 3791
    let s2'' = if s2' < 0 then s2' + 2147483399 else s2'
    let z = s1'' - s2''
    let z' = if z < 1 then z + 2147483562 else z
    (z', StdGen (s1'', s2''))
    
let stdSplit ((StdGen (s1,s2)) as std) = 
    let new_s1 = if s1 = 2147483562 then 1 else s1 + 1
    let new_s2 = if s2 = 1 then 2147483398 else s2 - 1
    let (StdGen (t1,t2)) = snd (stdNext std) 
    let left = StdGen (new_s1, t2)
    let right = StdGen (t1, new_s2)
    (left,right)

let rec iLogBase b i = if i < b then 1 else 1 + iLogBase b (i / b)

let rec stdRange (l,h) rng =
    if l > h then stdRange (h,l) rng
    else
        let k = h - l + 1
        let b = 2147483561
        let n = iLogBase b k
        let rec f n acc g = 
            if n = 0 then (acc,g) 
            else let (x,g') = stdNext g in f (n-1) (x + acc * b) g'
        match (f n 1 rng) with (v, rng') -> (l + abs(v) % k, rng')


let range = stdRange
let split = stdSplit
let newSeed() = DateTime.Now.Ticks |> int |> mkStdGen


(*let rander s = Seq.unfold (fun st -> let (s,st') = stdNext st in Some (s, st')) s
500 |> mkStdGen |> rander |> Seq.take 20 |> List.map (fun s -> Console.Write(s.ToString()+" "))
Console.WriteLine();

let ranger s = Seq.unfold (fun st -> let (s,st') = stdRange (-10,10) st in Some (s, st')) s
500 |> mkStdGen |> ranger |> Seq.take 200 |> List.map (fun s -> Console.WriteLine(s))

let splitter s = Seq.unfold (fun st -> let (s0,s1) = stdSplit st in Some((s0,s1), s1)) s 
500 |> mkStdGen |> splitter |> Seq.take 20 |> List.map (printfn "%A")

Console.ReadKey()*)
