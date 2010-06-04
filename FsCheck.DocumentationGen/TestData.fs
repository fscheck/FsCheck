module TestData

open FsCheck

let chooseFromList xs = 
    gen { let! i = Gen.choose (0, List.length xs-1) 
          return (List.nth xs i) }
          
let chooseBool = 
    Gen.oneof [ gen { return true }; gen { return false } ]

let chooseBoolFrequency = 
    Gen.frequency [ (2, gen { return true }); (1, gen { return false })]

//The size of test data
let matrix gen = Gen.sized <| fun s -> Gen.resize (s|>float|>sqrt|>int) gen

//Generating Recusrive data types
type Tree = Leaf of int | Branch of Tree * Tree

let rec unsafeTree() = 
    Gen.oneof [ Gen.map Leaf Arb.generate<int> 
                Gen.map2 (fun x y -> Branch (x,y)) (unsafeTree()) (unsafeTree())]

let tree =
    let rec tree' s = 
        match s with
        | 0 -> Gen.map Leaf Arb.generate<int>
        | n when n>0 -> 
            let subtree = tree' (n/2)
            Gen.oneof [ Gen.map Leaf Arb.generate<int> 
                        Gen.map2 (fun x y -> Branch (x,y)) subtree subtree]
        | _ -> invalidArg "s" "Only positive arguments are allowed"
    Gen.sized tree'
  
type Box<'a> = Whitebox of 'a | Blackbox of 'a

let boxGen<'a> : Gen<Box<'a>> = 
    gen { let! a = Arb.generate
          return! Gen.elements [ Whitebox a; Blackbox a] }
    
type MyGenerators =
    static member Tree() =
        {new Arbitrary<Tree>() with
            override x.Generator = tree
            override x.Shrinker t = Seq.empty }
    static member Box() = Arb.fromGen boxGen
Arb.register<MyGenerators>() |> ignore

let RevRevTree (xs:list<Tree>) = 
    List.rev(List.rev xs) = xs
Check.Quick RevRevTree

let RevRevBox (xs:list<Box<int>>) = 
    List.rev(List.rev xs) = xs
    |> Prop.collect xs
Check.Quick RevRevBox