
module Properties

open FsCheck
open System

let revRevIsOrig xs = List.rev(List.rev xs) = xs
let revRevIsOrigInt (xs:list<int>) = List.rev(List.rev xs) = xs

let rec private ordered xs = 
    match xs with
    | [] -> true
    | [x] -> true
    | x::y::ys ->  (x <= y) && ordered (y::ys)
let rec private insert x xs = 
    match xs with
    | [] -> [x]
    | c::cs -> if x <= c then x::xs else c::(insert x cs)
 
open Prop 
                     
let Insert (x:int) xs = ordered xs ==> ordered (insert x xs)

let Eager a = a <> 0 ==> (1/a = 1/a)
Check.Quick Eager

let Lazy a = a <> 0 ==> (lazy (1/a = 1/a))
Check.Quick Lazy

let orderedList = Arb.from<list<int>> |> Arb.mapFilter List.sort ordered
let insertWithArb x = forAll orderedList (fun xs -> ordered(insert x xs))
Check.Quick insertWithArb

let ExpectDivideByZero() = throws<DivideByZeroException,_> (lazy (raise <| DivideByZeroException()))
Check.Quick ExpectDivideByZero

let timesOut (a:int) = 
    lazy
        if a>10 then
            while true do System.Threading.Thread.Sleep(1000)
            true
        else 
            true
    |> within 2000
    
let insertTrivial (x:int) xs = 
    ordered xs ==> (ordered (insert x xs))
    |> trivial (List.length xs = 0)
Check.Quick insertTrivial

//Classifying test cases
let insertClassify (x:int) xs = 
    ordered xs ==> (ordered (insert x xs))
    |> classify (ordered (x::xs)) "at-head"
    |> classify (ordered (xs @ [x])) "at-tail" 
Check.Quick insertClassify
    
//Collecting data values
let insertCollect (x:int) xs = 
    ordered xs ==> (ordered (insert x xs))
        |> collect (List.length xs)
Check.Quick insertCollect

//Combining observations
let insertCombined (x:int) xs = 
    ordered xs ==> (ordered (insert x xs))
        |> classify (ordered (x::xs)) "at-head"
        |> classify (ordered (xs @ [x])) "at-tail"
        |> collect (List.length xs)
Check.Quick insertCombined

let complex (m: int) (n: int) =
    let res = n + m
    (res >= m)    |@ "result > #1" .&.
    (res >= n)    |@ "result > #2" .&.
    (res < m + n) |@ "result not sum"
Check.Quick complex

let multiply (n: int, m: int) =
  let res = n*m
  sprintf "evidence = %i" res @| (
    "div1" @| (m <> 0 ==> lazy (res / m = n)),
    "div2" @| (n <> 0 ==> lazy (res / n = m)),
    "lt1"  @| (res > m),
    "lt2"  @| (res > n))
Check.Quick multiply

let multiplyAsList (n: int, m: int) =
  let res = n*m
  sprintf "evidence = %i" res @| [
    "div1" @| (m <> 0 ==> lazy (res / m = n));
    "div2" @| (n <> 0 ==> lazy (res / n = m));
    "lt1"  @| (res > m);
    "lt2"  @| (res > n)]
Check.Quick multiplyAsList