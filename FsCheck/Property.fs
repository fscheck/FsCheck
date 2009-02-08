#light

namespace FsCheck

[<AutoOpen>]
module Property

open System
open Generator
open Common
open TypeClass

///The result of one execution of a property.
type Result = 
    {   Ok : option<bool>
        Stamp : list<string>
        Arguments : list<obj> 
        Exc: option<Exception> }

let private result =
  { Ok        = None
  ; Stamp     = []
  ; Arguments = []
  ; Exc = None
  }

let internal failed = { result with Ok = Some false }

let internal exc e = { result with Ok = Some false; Exc = Some e }

let internal succeeded = { result with Ok = Some true }

let internal rejected = { result with Ok = None }

//A rose is a pretty tree
//Draw it and you'll see.
//A Rose<Result> is used to keep, in a lazy way, a Result and the possible shrinks for the value in the node.
//The fst value is the current Result, and the list contains the properties yielding possibly shrunk results.
//Each of those can in turn have their own shrinks. 
type Rose<'a> = 
    MkRose of Lazy<'a> * seq<Rose<'a>> with
        member x.Map f = 
            match x with MkRose (x,rs) -> MkRose (lazy (f x.Value), rs |> Seq.map (fun r -> r.Map f)) 

let private fmapRose f (a:Rose<_>) = a.Map f
   
//careful here: we can't pattern match as follows, as this will result in evaluation:
//let rec join (MkRose (Lazy (MkRose(x,ts)),tts)) 
//instead, define everything inside the MkRose, as a lazily evaluated expression. Haskell would use an irrefutable
//pattern here (is this possible using an active pattern? -> to investigate!) 
let rec private join (MkRose (r,tts)) =
    //bweurgh. Need to match twice to keep it lazy.
    let x = lazy (match r with (Lazy (MkRose (x,_))) -> x.Force())
    let ts = Seq.append (Seq.map join tts) <| seq { yield! match r with (Lazy (MkRose (_,ts))) -> ts }
    MkRose (x,ts) 
  //first shrinks outer quantification; makes most sense
  // first shrinks inner quantification: MkRose (x,(ts ++ Seq.map join tts))

type RoseBuilder() =
    member internal b.Return(x) : Rose<_> = 
        MkRose (lazy x,Seq.empty)
    member internal b.Bind(m, k) : Rose<_> = 
        join ( fmapRose k m )              



let private rose = new RoseBuilder()

let private liftRose f = fun r -> rose{ let! r' = r
                                        return f r' }

let private lazyRose x = MkRose (x,Seq.empty)

///Type synonym for a test result generator.
type Property = Gen<Rose<Result>>

type Testable<'a> =
    abstract Property : 'a -> Property

let property<'a> p = getInstance (typedefof<Testable<_>>, typeof<'a>) |> unbox<Testable<'a>> |> (fun t -> t.Property p)

let private promoteRose m = Gen (fun s r -> liftRose (fun (Gen m') -> m' s r) m)

///Property combinator to shrink an original value x using the shrinking function shrink:'a -> #seq<'a>, and the testable
///function pf. 
let shrinking shrink x pf : Property =
    //cache is important here to avoid re-evaluation of property
    let rec props x = MkRose (lazy (property (pf x)), shrink x |> Seq.map props |> Seq.cache)
    fmapGen join <| promoteRose (props x)
 
let private liftRoseResult t : Property = gen { return t }

let private liftResult (r:Result) : Property = 
    liftRoseResult <| rose { return r }
 
let private liftBool b = liftResult <| { result with Ok = Some b  }

let private mapRoseResult f :( _ -> Property) = fmapGen f << property

let private mapResult f = mapRoseResult (fmapRose f)

let private evaluate body a =
    let argument a res = { res with Arguments = (box a) :: res.Arguments }
    try 
        property (body a)
    with
        e -> liftResult (exc e)
    |> fmapGen (fmapRose (argument a))

///Quantified property combinator. Provide a custom test data generator to a property.
let forAll gn body : Property = 
    gen{let! a = gn
        return! evaluate body a }

///Quantified property combinator. Provide a custom test data generator to a property. 
///Shrink failing test cases using the given shrink function.
let forAllShrink gn shrink body : Property =
    gen{let! a = gn
        return! shrinking shrink a (fun a' -> evaluate body a')}

type Testable =
    static member Unit() =
        { new Testable<unit> with
            member x.Property _ = property rejected }
    static member Bool() =
        { new Testable<bool> with
            member x.Property b = liftBool b }
    static member Lazy() =
        { new Testable<Lazy<'a>> with
            member x.Property b =
                let promoteLazy (m:Lazy<_>) = 
                    Gen (fun s r -> join <| lazyRose (lazy (match m.Value with (Gen g) -> g s r)))
                promoteLazy (lazy (property b.Value)) }
    static member Result() =
        { new Testable<Result> with
            member x.Property res = liftResult res }
    static member Property() =
        { new Testable<Property> with
            member x.Property prop = prop }
    static member Gen() =
        { new Testable<Gen<'a>> with
            member x.Property gena = gen { let! a = gena in return! property a } }
    static member RoseResult() =
        { new Testable<Rose<Result>> with
            member x.Property rosea = gen { return rosea } } 
    static member Arrow() =
        { new Testable<('a->'b)> with
            member x.Property f = forAllShrink arbitrary shrink f }
   

///Conditional property combinator. Resulting property holds if the property after ==> holds whenever the condition does.
let (==>) = 
    let implies b a = if b then property a else property ()
    implies

///Expect exception 't when executing p. So, results in success if an exception of the given type is thrown, 
///and a failure otherwise.
let expectException<'t, 'a when 't :> exn> (p : Lazy<'a>) = 
    try p.Force() |> ignore ; false with :? 't -> true

let private label str a = 
    let add res = { res with Stamp = str :: res.Stamp } 
    mapResult add (property a)

///Classify test cases combinator. Test cases satisfying the condition are assigned the classification given.
let classify b name = if b then label name else property

///Count trivial cases property combinator. Test cases for which the condition is True are classified as trivial.
let trivial b = classify b "trivial"

///Collect data values property combinator. The argument of collect is evaluated in each test case, 
///and the distribution of values is reported, using any_to_string.
let collect v = label <| any_to_string v

///Property constructor. Constructs a property from a bool.
[<Obsolete("Please omit this function call: it's no longer necessary.")>]
let prop b = property b

///Lazy property constructor. Constructs a property from a Lazy<bool>.
[<Obsolete("Please omit this function call: it's no longer necessary.")>]
let propl b = property b

