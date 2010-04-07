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

[<AutoOpen>]
module Functions =

    [<StructuredFormatDisplay("{StructuredDisplayAsTable}")>]
    type Function<'a,'b when 'a : comparison> = 
        Function of ref<list<('a*'b)>> * ('a ->'b) with
        member x.Value = match x with Function (_,f) -> f
        member x.Table = match x with Function (table,_) -> !table
        member x.StructuredDisplayAsTable =
            let layoutTuple (x,y) = sprintf "%A->%A" x y
            x.Table 
            |> Seq.distinctBy fst 
            |> Seq.sortBy fst 
            |> Seq.map layoutTuple 
            |> String.concat "; "
            |> sprintf "{ %s }"
           
    let toFunction f = 
        let table = ref []
        Function (table,fun x -> let y = f x in table := (x,y)::(!table); y)
