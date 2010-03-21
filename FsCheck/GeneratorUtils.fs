(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2010 Kurt Schelfthout. All rights reserved.          **
**  http://www.codeplex.com/fscheck                                         **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

#light

namespace FsCheck

module GeneratorUtils =

   open Gen

   /// Shortcut for constructing an Arbitrary instance from a generator
   /// shrink and coarb will not be supported for this type
   let arbGen (gen: Gen<'a>) : Arbitrary<'a> =
       { new Arbitrary<'a>() with
           override x.Arbitrary = gen
       }

   /// Shortcut for constructing an Arbitrary instance from a generator and shrinker.
   /// coarb will not be supported for this type.
   let arbGenShrink (gen: Gen<'a>, shrinker: 'a -> seq<'a>): Arbitrary<'a> =
       { new Arbitrary<'a>() with
           override x.Arbitrary = gen
           override x.Shrink a = shrinker a
       }

   /// Shortcut for constructing an Arbitrary instance from a generator and shrinker.
   /// The values produced/consumed by the generator shrinker will bewrapped/unwrapped
   /// in another type.
   /// coarb will not be supported for this type.
   let arbGenShrinkWrap (gen: Gen<'a>, shrinker: 'a -> seq<'a>) (wrap:'a->'b) (unwrap: 'b->'a) : Arbitrary<'b> =
       { new Arbitrary<'b>() with
           override x.Arbitrary = gen |> map wrap
           override x.Shrink b = b |> unwrap |> shrinker |> Seq.map wrap
       }

   let filteredOf (gen: Gen<'a>, shrinker: 'a -> seq<'a>) (pred:'a->bool) : Gen<'a> * ('a -> seq<'a>) =
       (gen |> suchThat pred, shrinker >> Seq.filter pred )


   /// Return a generator and shrinker that is a 'filtered' version of an existing type
   /// where the arbitrary and shrink instances are looked up by type class.
   let filtered pred = filteredOf (arbitrary, shrink) pred

   /// Given a generator, produce another generator that has a size parameter
   /// equal to the sqrt of the current size parameter.
   /// Useful when producing lists of lists or similar.
   let resizeSqrt gen = sized <| fun s -> resize (s |> float |> sqrt |>int) gen
