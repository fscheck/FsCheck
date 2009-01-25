#light

namespace FsCheck

[<AutoOpen>]
module Property

open System
open Generator
open Common
open TypeClass

///The result of one execution of a property.
type Result = { ok : option<Lazy<bool>>
                stamp : list<string>
                arguments : list<obj> 
                exc: option<Exception> }

//let private nothing = { ok = None; stamp = []; arguments = []; exc = None }

let result =
  { ok        = None
  ; stamp     = []
  ; arguments = []
  ; exc = None
  }

let failed = { result with ok = Some (lazy false) }

let succeeded = { result with ok = Some (lazy true) }

let rejected = { result with ok = None }

type Rose<'a> = 
    MkRose of Lazy<'a> * seq<Rose<'a>> with
        member x.Map f = match x with MkRose (x,rs) -> MkRose (lazy (f <| x.Force()), Seq.map (fun (r:Rose<_>) -> r.Map f) rs) 

let fmapRose f (a:Rose<_>) = a.Map f
   
let rec join (MkRose (Lazy (MkRose(x,ts)),tts)) = 
    MkRose (x,(Seq.append (Seq.map join tts) ts)) //first shrinks outer quantification; makes most sense
  // first shrinks inner quantification: MkRose (x,(ts ++ Seq.map join tts))

type RoseBuilder() =
    member b.Return(x) : Rose<_> = 
        MkRose (lazy x,Seq.empty)
    member b.Bind(m, k) : Rose<_> = 
        join ( fmapRose k m )              

let rose = new RoseBuilder()

let liftRose f = fun r -> rose{ let! r' = r
                                return f r' }

type Prop = MkProp of Rose<Result>
let unProp (MkProp rose) = rose

type Property = Gen<Prop>

type Testable<'prop> =
    abstract Property : 'prop -> Property

let property<'a> p = getInstance (typedefof<Testable<_>>, typeof<'a>) |> unbox<Testable<'a>> |> (fun t -> t.Property p)

let promoteRose m = Gen (fun s r -> liftRose (fun (Gen m') -> m' s r) m)

///Property combinator to shrink an original value x using the shrinking function shrink:'a -> #seq<'a>, and the testable
///function pf. 
let shrinking shrink x pf : Property =
    let rec props x = MkRose (lazy (property (pf x)), shrink x |> Seq.map props |>Seq.cache)
    let promoted = (promoteRose (props x))
    fmapGen (MkProp << join << (fmapRose unProp)) promoted
 
let liftRoseResult t : Property = gen { return MkProp t }

//liftResult :: Result -> Property
let liftResult (r:Result) : Property = 
    liftRoseResult <| rose { return r }
 
////liftBool :: Bool -> Property
let liftBool b = liftResult <| { result with ok = Some (lazy b)  }

let liftLazyBool lb = liftResult <| { result with ok = Some lb }


//mapProp :: Testable prop => (Prop -> Prop) -> prop -> Property
let mapProp f :( _ -> Property) = fmapGen f << property

//mapRoseIOResult :: Testable prop => (Rose (IO Result) -> Rose (IO Result)) -> prop -> Property
let mapRoseResult f = mapProp (fun (MkProp t) -> MkProp (f t))

//mapResult :: Testable prop => (Result -> Result) -> prop -> Property
let mapResult f = mapRoseResult (fmapRose f)

///Quantified property combinator. Provide a custom test data generator to a property.
let forAll gn body : Property = 
    //let evaluate a = property a//let (Prop gen) = property a in gen
    let argument a res = { res with arguments = (box a) :: res.arguments }
    gen{let! a = gn
        let! res = 
            try 
                property (body a)
            with
                e -> gen { return MkProp <| rose { return { result with ok = Some (lazy false); exc = Some e }}}
        //return res }
        return fmapRose (argument a) (unProp res) |> MkProp }

///Quantified property combinator. Provide a custom test data generator to a property. 
///Shrink failing test cases using the given shrink function.
let forAllShrink gn shrink body : Property =
    let argument a res = { res with arguments = (box a) :: res.arguments }
    gen{let! a = gn
        let! res = shrinking shrink a (fun a' ->
            printfn "forAllShrink shrinking got %A" a'
            try 
                property (body a')
            with
                e -> gen { return MkProp <| rose { return { result with ok = Some (lazy false); exc = Some e }}}
            |> fmapGen (unProp >> fmapRose (argument a'))
            )
        return res }
        //return fmapRose (argument a) (unProp res) |> MkProp }

//let forAllShrink gn body = 
//    let failed res =
//        match res.ok with
//        | None -> false
//        | Some x -> not x.Value
//    
//    // return true if you can continue to shrink (i.e. ok = false)
//    let f x =
//        let (Gen v) = (evaluate (body x))
//        failed (v 0 (Random.newSeed()))
//    let argument a res = { res with arguments = (box a) :: res.arguments } in
//        Prop <|  gen { let! a = gn
//                       let! res = 
//                            try 
//                                evaluate (body a)
//                            with
//                                e -> gen { return { nothing with ok = Some (lazy false); exc = Some e }}
//                       let a2 = if failed res then Shrink.doShrink f a else a
//                       return (argument a2 res) }


type Testable =
    static member Unit() =
        { new Testable<unit> with
            member x.Property _ = property rejected }
    static member Bool() =
        { new Testable<bool> with
            member x.Property b = liftBool b }//result { nothing with ok = Some (lazy b) } }
    static member LazyBool() =
        { new Testable<Lazy<bool>> with
            member x.Property b = liftLazyBool b }//result { nothing with ok = Some b } }
    static member Result() =
        { new Testable<Result> with
            member x.Property res = liftResult res }
    static member Property() =
        { new Testable<Property> with
            member x.Property prop = prop }
    static member GenProp() =
        { new Testable<Gen<'a>> with
            member x.Property gena = gen { let! a = gena in return! property a } }
    static member RoseResult() =
        { new Testable<Rose<Result>> with
            member x.Property rosea = gen { return MkProp rosea } } 
    static member Arrow() =
        { new Testable<('a->'b)> with
            member x.Property f = forAllShrink arbitrary shrink f }
   

///Conditional property combinator. Resulting property holds if the property after ==> holds whenever the condition does.
let (==>) = 
    let implies b a = if b then property a else property ()
    implies

let private label str a = 
    let add res = { res with stamp = str :: res.stamp } 
    mapResult add (property a)
    //((property a).Map add)

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