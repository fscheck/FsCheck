(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2013 Kurt Schelfthout. All rights reserved.          **
**  https://github.com/kurtschelfthout/FsCheck                              **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

namespace FsCheck

module internal Common =

    open System.Collections.Generic

    ///Memoize the given function using the given dictionary
    let memoizeWith (memo:IDictionary<'a,'b>) (f: 'a -> 'b) =
        fun n ->
            lock memo 
                (fun _ -> 
                    match memo.TryGetValue n with
                    | true, res -> res
                    | _ ->
                        let res = f n
                        memo.Add(n,res)
                        res)

    ///Memoize the given function.
    let memoize f =
        let t = new Dictionary<_,_>()
        memoizeWith t f

    let flip f x y = f y x

    let curry f = fun a b -> f (a,b)

    let curry2 f = fun a b c -> f (a,b,c)

    let uncurry f = fun (a,b) -> f a b

    let uncurry2 f = fun (a,b,c) -> f a b c


    //the following three are from Don Syme's blog:
    //http://blogs.msdn.com/b/dsyme/archive/2009/11/08/equality-and-comparison-constraints-in-f-1-9-7.aspx
    let equalsOn f x (yobj:obj) =
        match yobj with
        | :? 'T as y -> (f x = f y)
        | _ -> false
 
    let hashOn f x =  hash (f x)
 
    let compareOn f x (yobj: obj) =
        match yobj with
        | :? 'T as y -> compare (f x) (f y)
        | _ -> invalidArg "yobj" "cannot compare values of different types"

    let (|MapContains|_|) key value =
        Map.tryFind key value
        
    module Option =
        let toObject(opt:_ option) = 
            match opt with
            | None -> null
            | Some a -> a
