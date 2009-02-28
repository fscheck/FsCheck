#light

namespace FsCheck.Checks

module Common = 

    open FsCheck.Common

    let Memoize (f:int->string) (a:int) = memoize f a = f a

    let Flip (f: char -> int -> string) a b = flip f a b = f b a

module Generator =
    
    open FsCheck
    open FsCheck.Generator
    
    let sample n gn  = 
        let rec sample i seed samples =
            if i = 0 then samples
            else sample (i-1) (Random.stdSplit seed |> snd) (generate 1000 seed gn :: samples)
        sample n (Random.newSeed()) []
    
    type Interval = Interval of int * int
    
    type IntervalGenerator =
        //generates an interval between two positive ints
        static member Interval() =
            { new Arbitrary<Interval>() with
                override  x.Arbitrary = 
                    gen { 
                        let! start,offset = two arbitrary
                        return Interval (abs start,abs start+abs offset)
                    }
             }
    registerGenerators<IntervalGenerator>()
    
    let Choose (Interval (l,h)) = 
        choose (l,h)
        |> sample 10
        |> List.for_all (fun v -> l <= v && v <= h)
     
    let private isIn l elem = List.exists ((=) elem) l
       
    let Elements (l:list<char>) =
        not l.IsEmpty ==> 
        lazy (  elements l
                |> sample 50
                |> List.for_all (isIn l))
    
    let Constant (v : char) =
        constant v
        |> sample 10
        |> List.for_all ((=) v)
    
    let Oneof (l:list<string>) =
        not l.IsEmpty ==> 
        lazy (  List.map constant l
                |> oneof
                |> sample 50
                |> List.for_all (isIn l))
    
    let Frequency (l:list<int*string>) =
        let generatedValues = l |> List.filter (fst >> (<>) 0) |> List.map snd
        (sprintf "%A" generatedValues) @|
        (not generatedValues.IsEmpty ==>
         lazy ( List.map (fun (freq,s) -> (abs freq,constant s)) l
                |> frequency
                |> sample 100
                |> List.for_all (isIn generatedValues)))
    
    let LiftGen (f:string -> int) v =
        constant v
        |> liftGen f
        |> sample 10
        |> List.for_all ((=) (f v))
    