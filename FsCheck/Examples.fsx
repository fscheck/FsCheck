#light

#r "FSharp.Powerpack.dll"
#load "Random.fs"
#load "FsCheck.fs"

open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Collections
open System.Collections.Generic;
open FsCheck

//-------A Simple Example----------

//short version, also 'truly' generic (i.e. will get lists of ints, chars, floats,...)
let prop_RevRev xs = List.rev(List.rev xs) = xs
quickCheck prop_RevRev 

//long version: define your own generator (constraint to list<int>)
let lprop_RevRev =     
    forAll (Gen.List(Gen.Int)) (fun xs -> List.rev(List.rev xs) = xs |> propl)    
quickCheck prop_RevRev 

//this style is obsolote
//advantages:  generators are swapped more easily (are they? forall already does this...)
//let prop_RevRev' (xs:list<int>) = List.rev (List.rev xs) = xs  |> propl  
//qcheck (Gen.List(Gen.Int)) prop_RevRev'    

let prop_RevId xs = List.rev xs = xs
quickCheck prop_RevId

//-----Properties----------------
let prop_RevRevInt (xs:list<int>) = List.rev(List.rev xs) = xs
quickCheck prop_RevRev 

//Conditional Properties
let rec ordered xs = match xs with
                     | [] -> true
                     | [x] -> true
                     | x::y::ys ->  (x <= y) && ordered (y::ys)
let rec insert x xs = match xs with
                      | [] -> [x]
                      | c::cs -> if x <= c then x::xs else c::(insert x cs)                      
let prop_Insert (x:int) xs = ordered xs ==> propl (ordered (insert x xs))
quickCheck prop_Insert

let prop_Eager a = a <> 0 ==> propl (1/a = 1/a)
let prop_Lazy a = a <> 0 ==> prop (lazy (1/a = 1/a))