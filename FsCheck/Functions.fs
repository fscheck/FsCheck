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

[<AutoOpen>]
module public Functions =

    open Microsoft.FSharp.Text.StructuredFormat
    open Microsoft.FSharp.Text.StructuredFormat.LayoutOps

    [<StructuredFormatDisplay("{StructuredDisplayAsTable}")>]
    type Function<'a,'b when 'a : comparison> = 
        Function of ref<list<('a*'b)>> * ('a ->'b) with
        member x.Value = match x with Function (_,f) -> f
        member x.Table = match x with Function (table,_) -> !table
        member x.StructuredDisplayAsTable =
            let layoutTuple (x,y) = objL (box x) ++ sepL "->" ++ objL (box y)
            x.Table 
            |> Seq.distinctBy fst 
            |> Seq.sortBy fst 
            |> Seq.map (layoutTuple) 
            |> Seq.toList
            |> semiListL |> braceL
            |> Display.layout_to_string FormatOptions.Default
           
    let toFunction f = 
        let table = ref []
        Function (table,fun x -> let y = f x in table := (x,y)::(!table); y)
