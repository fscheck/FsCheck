﻿(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2015 Kurt Schelfthout and contributors.              **
**  All rights reserved.                                                    **
**  https://github.com/fscheck/FsCheck                              **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

namespace FsCheck

module internal Common =

    open System
#if PCL
    open System.Collections.Generic

    ///Create the Memoizer
    let memoizer () = new Dictionary<_,_>()

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
#else
    open System.Collections.Concurrent

    ///Create the Memoizer
    let memoizer () = new ConcurrentDictionary<_,_>()

    ///Memoize the given function using the given dictionary
    let memoizeWith (memo:ConcurrentDictionary<'a,'b>) (f: 'a -> 'b) =
        let ff = Func<'a,'b>(f)
        fun n ->
            memo.GetOrAdd (n, ff)
#endif

    ///Memoize the given function.
    let memoize f =
        memoizeWith (memoizer ()) f
    let flip f x y = f y x

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
