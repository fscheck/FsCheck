#light

namespace FsCheck

[<AutoOpen>]
module Property

open System
open Generator
open TypeClass

///The result of one execution of a property.
type Result = { ok : option<Lazy<bool>>
                stamp : list<string>
                arguments : list<obj> 
                exc: option<Exception> }

let private nothing = { ok = None; stamp = []; arguments = []; exc = None }

///A property that can be checked.
type Property = private Prop of Gen<Result>

let private result res = gen { return res } |> Prop

type Testable<'prop> =
    abstract Property : 'prop -> Property

let property<'a> p = getInstance (typedefof<Testable<_>>) (typeof<'a>) |> unbox<Testable<'a>> |> (fun t -> t.Property p)

let internal evaluate a = let (Prop gen) = property a in gen

///Quantified property combinator. Provide a custom test data generator to a property.
let forAll gn body = 
    let argument a res = { res with arguments = (box a) :: res.arguments } in
    Prop <|  gen {  let! a = gn
                    let! res = 
                        try 
                            (evaluate (body a))
                        with
                            e -> gen { return { nothing with ok = Some (lazy false); exc = Some e }}
                    return (argument a res) }

newTypeClass<Testable<_>>

type Testable =
    static member Unit() =
        { new Testable<unit> with
            member x.Property _ = property nothing }
    static member Bool() =
        { new Testable<bool> with
            member x.Property b = result { nothing with ok = Some (lazy b) } }
    static member LazyBool() =
        { new Testable<Lazy<bool>> with
            member x.Property b = result { nothing with ok = Some b } }
    static member Result() =
        { new Testable<Result> with
            member x.Property res = result res }
    static member Property() =
        { new Testable<Property> with
            member x.Property prop = prop }
    static member Arrow() =
        { new Testable<('a->'b)> with
            member x.Property f = forAll arbitrary f }

do registerInstances<Testable<_>,Testable>()
       
let private implies b a = if b then property a else property ()
///Conditional property combinator. Resulting property holds if the property after ==> holds whenever the condition does.
let (==>) b a = implies b a

let private label str a = 
    let add res = { res with stamp = str :: res.stamp } in
    Prop ((evaluate a).Map add)

///Classify test cases combinator. Test cases satisfying the condition are assigned the classification given.
let classify b name = if b then label name else property

///Count trivial cases property combinator. Test cases for which the condition is True are classified as trivial.
let trivial b = classify b "trivial"

///Collect data values property combinator. The argument of collect is evaluated in each test case, 
///and the distribution of values is reported, using any_to_string.
let collect v = label <| any_to_string v

/////Property constructor. Constructs a property from a bool.
[<Obsolete("Please omit this function call: it's no longer necessary.")>]
let prop b = property b//gen { return {nothing with ok = Some b}} |> Prop
/////Lazy property constructor. Constructs a property from a Lazy<bool>.
[<Obsolete("Please omit this function call: it's no longer necessary.")>]
let propl b = property b //gen { return {nothing with ok = Some (lazy b)}} |> Prop