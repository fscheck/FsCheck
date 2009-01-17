#light

namespace FsCheck

module Shrink

open System
open Random
open Reflect

let debug = false

type ShrinkType = (obj -> bool) -> obj -> option<obj>

let shrinkMap : Ref<Map<string, Lazy<ShrinkType>>> = ref (Map.empty)


let rec children0 (seen : Set<string>) (tFind : Type) (t : Type) : (obj -> list<obj>) =
            if tFind = t then
                fun o -> [o]
            else
                children1 seen tFind t

        and children1 (seen : Set<string>) (tFind : Type) (t : Type) =
            let ts = t.ToString();
            let seen2 = seen.Add ts
            if seen.Contains ts then
                fun _ -> []
            elif t.IsArray then
                let f = children0 seen2 tFind (t.GetElementType())
                fun o ->
                    let x = o :?> Array
                    List.concat [ for i in 0 .. x.Length-1 -> f (x.GetValue i) ]
            elif isUnionType t then
                let read = getUnionTagReader t
                let mp =
                    Map.of_array
                        [| for _,(tag,ts,_,destroy) in getUnionCases t ->
                            let cs = [ for u in ts -> children0 seen2 tFind u ]
                            tag, fun o -> List.concat <| List.map2 (<|) cs (List.of_array <| destroy o)
                        |]
                fun o -> mp.[read o] o
            elif isRecordType t then
                let destroy = getRecordReader t
                let cs = [ for i in getRecordFields t -> children0 seen2 tFind i.PropertyType ]
                fun o -> List.concat <| List.map2 (<|) cs (List.of_array <| destroy o)
            else
                fun _ -> []

let hunt (t : Type) : ShrinkType =
        let cs = children1 Set.empty t t
        fun test o ->
            let rec f xs =
                match xs with
                | [] -> None
                | x :: xs -> if test x then Some x else f xs
            let v = f (cs o)
            v


let rec getShrink (t : Type) : Lazy<ShrinkType> =
        if (debug) then printfn "getShrink type %A" t
        let ts = t.ToString()
        match (!shrinkMap).TryFind ts with
        | Some v -> v
        | None ->
            let res = lazy genShrink t
            shrinkMap := (!shrinkMap).Add (ts,res)
            res

    and arrayShrink (g : int -> Lazy<ShrinkType>) =
        fun test (o : obj) ->
            let cs = o :?> Array
            let mutable changed = false
            for i in 0 .. cs.Length - 1 do
                let old = cs.GetValue i
                match (g i).Value (fun o -> cs.SetValue(o, i); test (box cs)) (cs.GetValue i) with
                | None -> cs.SetValue(old, i)
                | Some x -> cs.SetValue(x, i); changed <- true
            if changed then Some (box cs) else None

    and listShrink (destroy : obj -> obj[]) (create : obj[] -> obj) (fields : list<Type>) =
        let ss = [| for i in fields -> getShrink i |]
        fun test o ->
            let cs = destroy o
            let mutable changed = false
            for i in 0 .. cs.Length - 1 do
                let old = cs.[i]
                match ss.[i].Value (fun o -> cs.[i] <- o; test (create cs)) cs.[i] with
                | None -> cs.[i] <- old
                | Some x -> cs.[i] <- x; changed <- true
            if changed then Some (create cs) else None


    // Try to shrink by first looking for children
    // Then if that doesn't work, try something more specific
    and genShrink (t : Type) : ShrinkType =
        let gen = hunt t
        let spec = newShrink t
        let f g x = match g x with None -> false,x | Some y -> true,y
        let rec fs g x = match g x with None -> false,x | Some y -> true,snd (fs g y)
        fun test o ->
            let b1,o = fs (gen test) o
            let b2,o = f (spec test) o
            if b1 || b2 then Some o else None

    and newShrink (t : Type) : ShrinkType =
        if t.IsArray then
            let t2 = t.GetElementType()
            let inner = getShrink t2
            fun test o ->
                let x = o :?> System.Array
                if x.Length = 0 then None
                else
                    let empty = box <| Array.CreateInstance(t2, 0)
                    if test empty then Some empty
                    else
                        let x2 = Array.CreateInstance(t2, x.Length)
                        Array.Copy(x, x2, x.Length)
                        arrayShrink (fun _ -> inner) test (box x2)

        
        elif isUnionType t then
            let read = getUnionTagReader t
            let mp =
                Map.of_array
                    [| for _,(tag,fields,create,destroy) in getUnionCases t ->
                        let ss = [| for t in fields -> getShrink t |]
                        tag, listShrink destroy create fields
                    |]
            fun test o -> mp.[read o] test o

        elif isRecordType t then
            let create = getRecordConstructor t
            let destroy = getRecordReader t
            let ts = [ for i in getRecordFields t -> i.PropertyType ]
            listShrink destroy create ts


        elif t = typeof<string> then
            
            fun test o ->
                let str = o |> unbox<string>
                let firstHalf (s:string) = s.Substring(1, s.Length-1) 
                if test (box "") then 
                    Some (box "")
                elif str.Length > 1 && test (str |> firstHalf |>box ) then
                    Some (str |> firstHalf |>box)
                else 
                    None
        
        else
            fun test o -> None



let shrinkObj (t : Type) =
    let f = (getShrink t).Value
    fun test x ->
        match f test x with
        | None -> x
        | Some y -> y


//a special case for union cases. Originally the shrink module used only static type (typeof<'a>) of union cases, and works for that.
//this has now changed to a dynamic type (x.GetType()). The upshot is that union cases are mapped to several classes at runtime, each
//of the cases as a subclass of the actual static type. However, the shrink module goes into stackoverflow if we pass it one of the 
//subclasses (not sure why...but that needs to be fixed...). In the short term, just translate a union type to its parent.
let staticTypeOf (t:Type) =
    if Microsoft.FSharp.Reflection.FSharpType.IsUnion t then t.BaseType else t

// you want to find the smallest value value such that
// test returns true, assuming (test x) = true
let shrink (test : 'a -> bool) (x : 'a) : 'a =
    //let f = unbox >> test
    if (debug) then printfn "shrinking value of runtime type %A static type %A" (x.GetType()) (typeof<'a>)
    let f = fun x -> let v = test (unbox x) 
                     //printf "%s" (if v then "#" else ".")
                     v
    //unbox <| shrinkObj (typeof<'a>) (f) (box x)
    unbox <| shrinkObj (staticTypeOf <| x.GetType()) (f) (box x)