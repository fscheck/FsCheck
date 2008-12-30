#light

namespace FSCheck

open System

open FSCheck.FSCheck
open FSCheck.Common
open FSCheck.Reflect
open FSCheck.Random

// Only for tuples
open Microsoft.FSharp.Reflection
open System.Reflection

module GenReflect = 

    let debugNeilCheck = false

    // first function given is impure
    type NeilGen = (int -> int -> int) -> int -> obj

    let genMap : Ref<Map<string, Lazy<NeilGen>>> = ref (Map.empty)



    let rec getNeilGen (t : Type) : Lazy<NeilGen> =
            let ts = t.ToString()
            match (!genMap).TryFind ts with
            | Some v -> v
            | None ->
                let res = lazy neilGen t
                genMap := (!genMap).Add (ts,res)
                res

        //this finds the generators for each of the types, then chooses one element for each type (so, a product type like tuples)
        and productGen (ts : list<Type>) =
            let gs = [| for t in ts -> getNeilGen t |]
            let n = gs.Length
            fun next size -> [| for g in gs -> g.Value next ((size / n) - 1) |]

        and intGen next size = next (-size) size
        and charGen next size = Char.chr (next 32 127)

        and neilGen (t : Type) : NeilGen =
            if t.IsArray then
                let t2 = t.GetElementType()
                let inner = getNeilGen t2
                fun next size ->
                    let n = max 0 (next 0 size)
                    let res = Array.CreateInstance(t2, n)
                    for i in 0 .. n-1 do
                        res.SetValue(inner.Value next (size - 1), i)
                    box res

            //this is for lists; based on the generator for arrays (turns an array into a list using reflection)
            elif genericTypeEq t (typeof<List<unit>>) then
                let t2 = (t.GetGenericArguments()).[0]
                let inner = getNeilGen (t2.MakeArrayType())
                
                let modu = t.Assembly.GetType "Microsoft.FSharp.Collections.ListModule"
                let meth = modu.GetMethod "of_array"
                let of_array = meth.MakeGenericMethod [| t2 |]
                
                fun next size -> box <| of_array.Invoke(null, [| inner.Value next size |])

            
            elif isTupleType t then
                let ts = match FSharpType.IsTuple t with 
                         | true -> FSharpType.GetTupleElements t |> List.of_seq
                         | _ -> failwith "not a tuple, but should be"
                let g = productGen ts
                let create = FSharpValue.PrecomputeRecordConstructor t
                fun next size -> create (g next size)


            elif isRecordType t then
                let g = productGen [ for pi in getRecordFields t -> pi.PropertyType ]
                let create = getRecordConstructor t
                fun next size -> create (g next size)


            elif isUnionType t then
                // figure out the "size" of a union
                // 0 = nullary, 1 = non-recursive, 2 = recursive
                let unionSize (ts : list<Type>) : int =
                    if ts.IsEmpty then 0 else
                        let tsStar = List.concat (ts :: List.map containedTypes ts) //containedTypes is not implemented, always returns[]
                        if List.exists(fun (x : Type) -> x.ToString() = t.ToString()) tsStar then 2 else 1
                        //so this wil either return 0 or 1, never 2...
                        
                let unionGen create ts =
                    let g = productGen ts
                    fun next size -> create (g next size)

                let gs = [| for _,(_,fields,create,_) in getUnionCases t -> unionSize fields, unionGen create fields |]
                let lowest = Array.reduce_left min <| Array.map fst gs
                let small = [| for i,g in gs do if i = lowest then yield g |]
                let large = [| for _,g in gs -> g |]
                fun next size ->
                    let gs = if size <= 0 then small else large
                    gs.[next 0 (gs.Length-1)] next size


            elif t = typeof<string> then
                let inner = getNeilGen (typeof<char[]>)
                fun next size -> box <| new String(unbox (inner.Value next size) : char[])

            elif t = typeof<float> then
                fun next size ->
                    let fraction a b c = double a + ( double b / abs (double c)) + 1.0 
                    let value() = intGen next size
                    box <| fraction (value()) (value()) (value())

            elif t = typeof<unit> then
                fun next size -> box ()
            elif t = typeof<int> then
                fun next size -> box <| intGen next size
            elif t = typeof<char> then
                fun next size -> box <| charGen next size
            else
                failwithf "Geneflect.neilGen, type not handled %A" t


    //these two do the trick of converting a type into a generator for that type; in fact all of the above can be integrated
    //as is. Cool.
    let geneflectObj (t : Type) : Gen<obj> = Gen <| fun size stdgen ->
        if debugNeilCheck then printfn "%A" size
        let gen = ref stdgen
        let next low high =
            let v,g = range (low,high) !gen
            gen := g
            v
        (getNeilGen t).Value next size


    let geneflect() : Gen<'a> = (geneflectObj (typeof<'a>)).Map unbox
