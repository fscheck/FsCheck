#light

namespace FsCheck

module internal Common

let memoize (f: 'a -> 'b) =
    let t = new System.Collections.Generic.Dictionary<'a,'b>()
    fun n ->
        if t.ContainsKey(n) then t.[n]
        else let res = f n
             t.Add(n,res)
             res

let (|Lazy|) (inp:Lazy<'a>) = inp.Force()