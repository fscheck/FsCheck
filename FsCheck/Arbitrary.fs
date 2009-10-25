(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2009 Kurt Schelfthout. All rights reserved.          **
**  http://www.codeplex.com/fscheck                                         **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

#light

namespace FsCheck

module Arbitrary =

    open TypeClass
    open Generator
    open System
    open ReflectArbitrary
    open Functions
    open GeneratorUtils

    /// Generate an array of a specified size and generator
    let arrayOfSize (g: Gen<'a>) n : Gen<'a[]> = vector g n |> liftGen Array.ofList

    /// Generate an array using the specified generator
    let arrayOf (g: Gen<'a>) : Gen<'a[]> = gen {
       let! size = sized <| fun n -> choose(0, n)
       return! arrayOfSize g size
    }

    /// Generate a 2D array of a specified size
    // TODO: Rescale appropriately
    let array2DOfSize (g: Gen<'a>) (rows: int) (cols: int) : Gen<'a[,]> = gen {
       let! arr1 = arrayOfSize g (rows * cols)
       return Array2D.init rows cols (fun r c -> arr1.[cols*r + c])
    }

    /// Generate a 2D array of an arbitrary size
    // TODO: Rescale appropriately
    let array2DOf (g: Gen<'a>) : Gen<'a[,]> = gen {
       let! rows = sized <| fun n -> choose(0, n)
       let! cols = sized <| fun n -> choose(0, n)
       return! array2DOfSize g rows cols
    }

    /// Generate a set using the given generator
    let setOf (g: Gen<'a>) : Gen<Set<'a>> = listOf g |> liftGen Set.ofList

    /// Generator/shrinker for non-negative integers
    let nonNegativeInt, shrinkNonNegativeInt = filtered ((>=) 0)

    /// Generator/shrinker for non-negative integers
    let positiveInt, shrinkPositiveInt = filtered ((>) 0)

    let nonZeroInt, shrinkNonZeroInt = filtered ((<>) 0)

    let stringNoNulls, shrinkStringNoNulls = filtered (fun s -> not(String.exists (fun c -> c = '\000') s))

    let nonEmptyString, shrinkNonEmptyString =
       filtered (fun s -> s <> "" && not (String.exists (fun c -> c ='\000') s))

    /// Generate a subset of an existing set
    let subsetOf (s: Set<'a>) : Gen<Set<'a>> =
       gen { // Convert the set into an array
             let setElems: 'a[] = Array.ofSeq s
             // Generate indices into the array (up to the number of elements)
             let! size = choose(0, s.Count)
             let! indices = arrayOfSize (choose(0, s.Count-1)) size
             // Extract the elements
             let arr: 'a[] = indices |> Array.map (fun i -> setElems.[i])
             // Construct a set (which eliminates dups)
             return Set.ofArray arr }

    /// Generate a non-empty subset of an existing (non-empty) set
    let nonEmptySubsetOf (s: Set<'a>) : Gen<Set<'a>> =
       gen { // Convert the set into an array
             let setElems: 'a[] = Array.ofSeq s
             // Generate indices into the array (up to the number of elements)
             let! size = choose(1, s.Count)
             let! indices = arrayOfSize (choose(0, s.Count-1)) size
             // Extract the elements
             let arr: 'a[] = indices |> Array.map (fun i -> setElems.[i])
             // Construct a set (which eliminates dups)
             return Set.ofArray arr }

    /// Shrink the size of set, ensuring it is still non-empty
    let shrinkNonEmptySetSize (s: Set<'a>) : seq<Set<'a>> =
       Seq.map (fun a -> Set.remove a s) s |> Seq.filter (fun s -> s.Count > 0)

    /// Generate a set
    let set() = arbitrary |> fmapGen Set.ofList
    /// Shrink a set
    let shrinkSet s = s |> Set.toList |> shrink |> Seq.map Set.ofList

    /// Generate a map
    let map() = arbitrary |> fmapGen Map.ofList
    /// Shrink a map
    let shrinkMap m = m |> Map.toList |> shrink |> Seq.map Map.ofList

    let private nonEmptySetGenShrink() = filtered (fun s -> Set.count s > 0)
    /// Generate a non-empty set
    let nonEmptySet() = fst <| nonEmptySetGenShrink()
    /// Shrink a non-empty set, keeping it non-empty
    let shrinkNonEmptySet s = (snd <| nonEmptySetGenShrink()) s

    let private nonEmptyArrayGenShrink() = filtered (fun a -> Array.length a > 0)
    /// Generate a non-empty array
    let nonEmptyArray() = fst <| nonEmptyArrayGenShrink()
    /// Shrink a non-empty array, keeping it non-empty
    let shrinkNonEmptyArray a = (snd <| nonEmptyArrayGenShrink()) a

    /// Shrink a fixed-size array by shrinking data values only
    let shrinkFixedSizeArray a = a |> Seq.mapi (fun i x -> shrink x |> Seq.map (fun x' ->
                                                       let data' = Array.copy a
                                                       data'.[i] <- x'
                                                       data')
                                               ) |> Seq.concat

    
    /// Generate a random enum of the type specified by the type parameter
    let enumOf<'enumType when 'enumType :> Enum>() : Gen<'enumType>  =
       liftGen unbox (ReflectArbitrary.enumOfType (typeof<'enumType>))

    type NonNegative = NonNegativeInt of int
    type PositiveInt = PositiveInt of int
    type NonZeroInt = NonZeroInt of int
    type NonEmptyString = NonEmptyString of string
    type StringNoNulls = StringNoNulls of string
    type NonEmptySet<'a when 'a : comparison> = NonEmptySet of Set<'a>
    type NonEmptyArray<'a> = NonEmptyArray of 'a[]
    type FixedSizeArray<'a> = FixedSizeArray of 'a[]

    /// Generate a date with no time-part
    let date = gen { let! PositiveInt yOffset = arbitrary
                     let y = 1900 + yOffset
                     let! m = choose(1, 12)
                     let! d = choose(1, DateTime.DaysInMonth(y, m))
                     return System.DateTime(y, m, d) }


    let private fraction (a:int) (b:int) (c:int) = 
        double a + double b / (abs (double c) + 1.0) 

    ///A collection of default generators.
    type Arbitrary() =
        ///Generates (), of the unit type.
        static member Unit() = 
            { new Arbitrary<unit>() with
                override x.Arbitrary = gen { return () } 
                override x.CoArbitrary _ = variant 0
            }
        ///Generates arbitrary bools.
        static member Bool() = 
            { new Arbitrary<bool>() with
                override x.Arbitrary = elements [true; false] 
                override x.CoArbitrary b = if b then variant 0 else variant 1
            }
        //byte generator contributed by Steve Gilham.
        ///Generates arbitrary bytes.
        static member Byte() =   
            { new Arbitrary<byte>() with  
                override x.Arbitrary = sized <| fun n ->   
                   let ig = if n > 255 then choose (0, 255) else choose (0,n)  
                   ig.Map byte  
                override x.CoArbitrary n = variant (if n >= byte(0) then 2*int(n) else 2*(- int(n)) + 1) 
                //TODO: check if something like int |> shrink |> byte works 
                override x.Shrink n =   
                    let (|>|) x y = abs (int(x)) > abs (int(y))  
                    seq {   if n < byte(0) then yield byte(-int(n))  
                            if n <> 0uy then yield byte(0)
                            yield! Seq.map byte (Seq.unfold (fun st -> let st = st / 2 in Some (n-(byte(st)), st)) (int(n))    
                                    |> Seq.takeWhile ((|>|) n)) }  
                    |> Seq.distinct  
            }  
        ///Generate arbitrary int that is between -size and size.
        static member Int() = 
            { new Arbitrary<int>() with
                override x.Arbitrary = sized <| fun n -> choose (-n,n) 
                override x.CoArbitrary n = variant (if n >= 0 then 2*n else 2*(-n) + 1)
                override x.Shrink n = 
                    let (|>|) x y = abs x > abs y 
                    seq {   if n < 0 then yield -n
                            if n <> 0 then yield 0 
                            yield! Seq.unfold (fun st -> let st = st / 2 in Some (n-st, st)) n 
                                    |> Seq.takeWhile ((|>|) n) }
                    |> Seq.distinct
            }
        ///Generates arbitrary floats, NaN, NegativeInfinity, PositiveInfinity, Maxvalue, MinValue, Epsilon included fairly frequently.
        static member Float() = 
            { new Arbitrary<float>() with
                override x.Arbitrary = 
                    frequency   [(6, liftGen3 fraction arbitrary arbitrary arbitrary)
                                ;(1, elements [ Double.NaN; Double.NegativeInfinity; Double.PositiveInfinity])
                                ;(1, elements [ Double.MaxValue; Double.MinValue; Double.Epsilon])]
                override x.CoArbitrary fl = 
                    let d1 = sprintf "%g" fl
                    let spl = d1.Split([|'.'|])
                    let m = if (spl.Length > 1) then spl.[1].Length else 0
                    let decodeFloat = (fl * float m |> int, m )
                    coarbitrary <| decodeFloat
                override x.Shrink fl =
                    let (|<|) x y = abs x < abs y
                    seq {   if Double.IsInfinity fl || Double.IsNaN fl then 
                                yield 0.0
                            else
                                if fl < 0.0 then yield -fl
                                let truncated = truncate fl
                                if truncated |<| fl then yield truncated }
                    |> Seq.distinct
            }
        ///Generates arbitrary chars, between ASCII codes Char.MinValue and 127.
        static member Char() = 
            { new Arbitrary<char>() with
                override x.Arbitrary = fmapGen char (choose (int Char.MinValue, 127))
                override x.CoArbitrary c = coarbitrary (int c)
                override x.Shrink c =
                    seq { for c' in ['a';'b';'c'] do if c' < c || not (Char.IsLower c) then yield c' }
            }
        ///Generates arbitrary strings, which are lists of chars generated by Char.
        static member String() = 
            { new Arbitrary<string>() with
                override x.Arbitrary = fmapGen (fun chars -> new String(List.toArray chars)) arbitrary
                override x.CoArbitrary s = s.ToCharArray() |> Array.toList |> coarbitrary
                override x.Shrink s = s.ToCharArray() |> Array.toList |> shrink |> Seq.map (fun chars -> new String(List.toArray chars))
            }
        ///Genereate a 2-tuple.
        static member Tuple2() = 
            { new Arbitrary<'a*'b>() with
                override x.Arbitrary = liftGen2 (fun x y -> (x,y)) arbitrary arbitrary
                //extra paranthesis are needed here, otherwise F# gets confused about the number of arguments
                //and doesn't correctly see that this really overriddes the right method
                override x.CoArbitrary ((a,b)) = coarbitrary a >> coarbitrary b
                override x.Shrink ((x,y)) = 
                    seq {   for x' in shrink x -> (x',y ) 
                            for y' in shrink y -> (x ,y') }
            }
        ///Genereate a 3-tuple.
        static member Tuple3() = 
            { new Arbitrary<'a*'b*'c>() with
                override x.Arbitrary = liftGen3 (fun x y z -> (x,y,z)) arbitrary arbitrary arbitrary
                override x.CoArbitrary ((a,b,c)) = coarbitrary a >> coarbitrary b >> coarbitrary c
                override x.Shrink ((x,y,z)) = 
                    seq {   for x' in shrink x -> (x',y ,z ) 
                            for y' in shrink y -> (x ,y',z ) 
                            for z' in shrink z -> (x ,y ,z') }
            }
        ///Genereate a 4-tuple.
        static member Tuple4() = 
            { new Arbitrary<'a*'b*'c*'d>() with
                override x.Arbitrary = liftGen4 (fun x y z u-> (x,y,z,u)) arbitrary arbitrary arbitrary arbitrary
                override x.CoArbitrary ((a,b,c,d)) = coarbitrary a >> coarbitrary b >> coarbitrary c >> coarbitrary d
                override x.Shrink ((x,y,z,u)) = 
                    seq {   for x' in shrink x -> (x',y ,z ,u ) 
                            for y' in shrink y -> (x ,y',z ,u ) 
                            for z' in shrink z -> (x ,y ,z',u ) 
                            for u' in shrink u -> (x ,y ,z ,u')}
            }
        ///Genereate a 5-tuple.
        static member Tuple5() = 
            { new Arbitrary<'a*'b*'c*'d*'e>() with
                override x.Arbitrary = liftGen5 (fun x y z u v-> (x,y,z,u,v)) arbitrary arbitrary arbitrary arbitrary arbitrary
                override x.CoArbitrary ((a,b,c,d,e)) = coarbitrary a >> coarbitrary b >> coarbitrary c >> coarbitrary d >> coarbitrary e
                override x.Shrink ((x,y,z,u,v)) = 
                    seq {   for x' in shrink x -> (x',y ,z ,u ,v ) 
                            for y' in shrink y -> (x ,y',z ,u ,v ) 
                            for z' in shrink z -> (x ,y ,z',u ,v ) 
                            for u' in shrink u -> (x ,y ,z ,u',v )
                            for v' in shrink v -> (x ,y ,z ,u ,v') }
            }
        ///Genereate a 6-tuple.
        static member Tuple6() = 
            { new Arbitrary<'a*'b*'c*'d*'e*'f>() with
                override x.Arbitrary = 
                    liftGen6 (fun x y z u v w-> (x,y,z,u,v,w)) arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary
                override x.CoArbitrary ((a,b,c,d,e,f)) = 
                    coarbitrary a >> coarbitrary b >> coarbitrary c >> coarbitrary d >> coarbitrary e >> coarbitrary f
                override x.Shrink ((x,y,z,u,v,w)) = 
                    seq {   for x' in shrink x -> (x',y ,z ,u ,v ,w ) 
                            for y' in shrink y -> (x ,y',z ,u ,v ,w ) 
                            for z' in shrink z -> (x ,y ,z',u ,v ,w ) 
                            for u' in shrink u -> (x ,y ,z ,u',v ,w )
                            for v' in shrink v -> (x ,y ,z ,u ,v',w )
                            for w' in shrink w -> (x ,y ,z ,u ,v ,w') }
            }
        ///Generate an option value that is 'None' 1/4 of the time.
        static member Option() = 
            { new Arbitrary<option<'a>>() with
                override x.Arbitrary = frequency [(1, gen { return None }); (3, liftGen Some arbitrary)]
                override x.CoArbitrary o = 
                    match o with 
                    | None -> variant 0
                    | Some y -> variant 1 >> coarbitrary y
                override x.Shrink o =
                    match o with
                    | Some x -> seq { yield None; for x' in shrink x -> Some x' }
                    | None  -> Seq.empty
            }
        ///Generate a list of values. The size of the list is between 0 and the test size + 1.
        static member FsList() = 
            { new Arbitrary<list<'a>>() with
                override x.Arbitrary = sized (fun n -> gen.Bind(choose(0,n+1 (*avoid empties*)), vector arbitrary))
                override x.CoArbitrary l = 
                    match l with
                    | [] -> variant 0
                    | x::xs -> coarbitrary x << variant 1 << coarbitrary xs
                override x.Shrink l =
                    match l with
                    | [] ->         Seq.empty
                    | (x::xs) ->    seq { yield xs
                                          for xs' in shrink xs -> x::xs'
                                          for x' in shrink x -> x'::xs }
            }
        ///Generate an object.
        static member Object() =
            { new Arbitrary<obj>() with
                override x.Arbitrary = 
                    oneof [ fmapGen box <| arbitrary<char>; fmapGen box <| arbitrary<string>; fmapGen box <| arbitrary<bool> ]
                override x.CoArbitrary o = 
                    match o with
                    | :? char as c -> variant 0 >> coarbitrary c
                    | :? string as s -> variant 1 >> coarbitrary s
                    | :? bool as b -> variant 2 >> coarbitrary b
                    | _ -> failwith "Unknown domain type in coarbitrary of obj"
                override x.Shrink o =
                    seq {
                        match o with
                        | :? char as c -> yield box true; yield box false; yield! shrink c |> Seq.map box
                        | :? string as s -> yield box true; yield box false; yield! shrink s |> Seq.map box
                        | :? bool as b -> yield! Seq.empty
                        | _ -> failwith "Unknown type in shrink of obj"
                    }
            }
        //Generate a rank 1 array.
        static member Array() =
            { new Arbitrary<'a[]>() with
                override x.Arbitrary = arbitrary |> fmapGen List.toArray
                override x.CoArbitrary a = a |> Array.toList |> coarbitrary
                override x.Shrink a = a |> Array.toList |> shrink |> Seq.map List.toArray
            }
         ///Generate a function value.
        static member Arrow() = 
            { new Arbitrary<'a->'b>() with
                override x.Arbitrary = promote (fun a -> coarbitrary a arbitrary)
                override x.CoArbitrary f = 
                    (fun gn -> gen {let x = arbitrary
                                    return! coarbitrary (fmapGen f x) gn }) 
            }
        static member Function() =
            { new Arbitrary<Function<'a,'b>>() with
                override x.Arbitrary = fmapGen toFunction arbitrary
                override x.Shrink f = 
                    let update x' y' f x = if x = x' then y' else f x
                    seq { for (x,y) in f.Table do 
                            for y' in shrink y do 
                                yield toFunction (update x y' f.Value) }
            }
        static member DateTime() = arbGen date
        static member Array2D() = arbGen <| array2DOf arbitrary
        static member NonNegativeInt() =
           arbGenShrinkWrap (nonNegativeInt, shrinkNonNegativeInt) NonNegativeInt (fun (NonNegativeInt n) -> n)
        static member PositiveInt() =
           arbGenShrinkWrap (positiveInt, shrinkPositiveInt) PositiveInt (fun (PositiveInt n) -> n)
        static member NonZeroInt() =
           arbGenShrinkWrap (nonZeroInt, shrinkNonZeroInt) NonZeroInt (fun (NonZeroInt n) -> n)
        static member StringNoNulls() =
           arbGenShrinkWrap (stringNoNulls, shrinkStringNoNulls) StringNoNulls (fun (StringNoNulls s) -> s)
        static member NonEmptyString() =
           arbGenShrinkWrap (nonEmptyString, shrinkNonEmptyString) NonEmptyString (fun (NonEmptyString s) -> s)
        static member Set() = arbGenShrink (set(), shrinkSet)
        static member Map() = arbGenShrink (map(), shrinkMap)

        static member NonEmptyArray() =
           arbGenShrinkWrap (nonEmptyArrayGenShrink()) NonEmptyArray (fun (NonEmptyArray s) -> s)
        static member NonEmptySet() =
           arbGenShrinkWrap (nonEmptySetGenShrink()) NonEmptySet (fun (NonEmptySet s) -> s)
        static member FixedSizeArray() =
           arbGenShrinkWrap (arbitrary, shrinkFixedSizeArray) FixedSizeArray (fun (FixedSizeArray a) -> a)
        static member CatchAll() =
            { new Arbitrary<'a>() with
                override x.Arbitrary = reflectGen
                override x.Shrink a = reflectShrink a
            }
