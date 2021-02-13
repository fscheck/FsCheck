﻿namespace FsCheck.Internals

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


    // The following function is taken from Tomas Petricek's stackoverflow answer:
    // http://stackoverflow.com/a/12564899/2219351
    /// Returns a sequence that, when iterated, 
    /// yields elements of the underlying sequence while the given predicate returns true, 
    /// and then returns the last element, but none further.
    let takeWhilePlusLast predicate (s:seq<_>) = 
        /// Iterates over the enumerator, yielding elements and
        /// stops after an element for which the predicate does not hold
        let rec loop (en:System.Collections.Generic.IEnumerator<_>) = seq {
            if en.MoveNext() then
                // Always yield the current, stop if predicate does not hold
                yield en.Current
            if predicate en.Current then
                yield! loop en }

        // Get enumerator of the sequence and yield all results
        // (making sure that the enumerator gets disposed)
        seq { use en = s.GetEnumerator()
              yield! loop en }

    /// Convert a ValueTuple to a Tuple.
    let ofValue (struct(x,y)) = x,y

    let inline isPowerOf2 n =
        (n <> LanguagePrimitives.GenericZero) && 
        ((n &&& (n - LanguagePrimitives.GenericOne)) = LanguagePrimitives.GenericZero) 

    //  !!! NOTE !!!
    //  ----------------
    //  This attribute is required to properly expose extension methods to VB consumers. 
    //  It only needs to appear _once_ in the entire assembly -- but it is needed.
    [<assembly: System.Runtime.CompilerServices.Extension>]
    do ()
