#light

namespace FsCheck

module internal Common

//generic memoize function from Expert F# book
let memoize (f: 'a -> 'b) =
    let t = new System.Collections.Generic.Dictionary<'a,'b>()
    fun n ->
        if t.ContainsKey(n) then t.[n]
        else let res = f n
             t.Add(n,res)
             res

//and 'a ``lazy`` = Lazy<'a>

//used to be in FSharp libs
let (|Lazy|) (inp:Lazy<'a>) = inp.Force()
    
let inline fmap (f:^c -> ^d) (a:^a) : ^b = (^a : (member Map: (^c -> ^d) -> ^b) (a,f))