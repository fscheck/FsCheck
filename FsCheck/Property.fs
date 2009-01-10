#light

namespace FsCheck

[<AutoOpen>]
module Property

open System
open Generator

///The result of one execution of a property.
type Result = { ok : option<Lazy<bool>>
                stamp : list<string>
                arguments : list<obj> 
                exc: option<Exception> }

let private nothing = { ok = None; stamp = []; arguments = []; exc = None }

///A property that can be checked.
type Property = private Prop of Gen<Result>

let private result res = gen { return res } |> Prop
                       
let internal evaluate (Prop gen) = gen

//let forAllShrink (gn : Gen<'a>) (body : 'a -> Property) : Property = 
//    let failed res =
//        match res.ok with
//        | None -> false
//        | Some x -> not x.Value
//    
//    // return true if you can continue to shrink (i.e. ok = false)
//    let f x =
//        let (Gen v) = (evaluate (body x))
//        failed (v 0 (newSeed()))
//
//    let argument a res = { res with arguments = lazy (any_to_string a :: res.arguments.Value) } in
//    Prop <|  gen { let! a = gn
//                   let! res = (evaluate (body a))
//                   let a2 = if failed res then shrink f a else a
//                   return (argument a2 res) }


///Quantified property combinator. Provide a custom test data generator to a property.
let forAll gn body = 
    let failed res =
        match res.ok with
        | None -> false
        | Some x -> not x.Value
    
    // return true if you can continue to shrink (i.e. ok = false)
    let f x =
        let (Gen v) = (evaluate (body x))
        failed (v 0 (Random.newSeed()))
    let argument a res = { res with arguments = (box a) :: res.arguments } in
        Prop <|  gen { let! a = gn
                       let! res = 
                            try 
                                evaluate (body a)
                            with
                                e -> gen { return { nothing with ok = Some (lazy false); exc = Some e }}
                       let a2 = if failed res then Shrink.shrink f a else a
                       return (argument a2 res) }
//    Prop <|  gen {  let! a = gn
//                    let! res = 
//                        try 
//                            evaluate (body a)
//                        with
//                            e -> gen { return { nothing with ok = Some (lazy false); exc = Some e }}
//                    //do printfn "
//                    let shrunk_a = if failed res then Shrink.shrink f a else a
//                    return (argument shrunk_a res) }

let private emptyProperty = result nothing

let private implies b a = if b then a else emptyProperty
///Conditional property combinator. Resulting property holds if the property after ==> holds whenever the condition does.
let (==>) b a = implies b a

let private label str a = 
    let add res = { res with stamp = str :: res.stamp } in
    Prop ((evaluate a).Map add)

///Classify test cases combinator. Test cases satisfying the condition are assigned the classification given.
let classify b name a = if b then label name a else a

///Count trivial cases property combinator. Test cases for which the condition is True are classified as trivial.
let trivial b = classify b "trivial"

///Collect data values property combinator. The argument of collect is evaluated in each test case, 
///and the distribution of values is reported, using any_to_string.
let collect v = label <| any_to_string v

///Property constructor. Constructs a property from a bool.
let prop b = gen { return {nothing with ok = Some b}} |> Prop
///Lazy property constructor. Constructs a property from a Lazy<bool>.
let propl b = gen { return {nothing with ok = Some (lazy b)}} |> Prop