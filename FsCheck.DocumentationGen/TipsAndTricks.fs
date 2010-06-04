module TipsAndTricks

open FsCheck

//Testing functions
open TestData
open Prop

let associativity (x:Tree) (f:Tree->float,g:float->char,h:char->int) = ((f >> g) >> h) x = (f >> (g >> h)) x
Check.Quick associativity

let mapRec (F (_,f)) (l:list<int>) =
    not l.IsEmpty ==>
        lazy (List.map f l = ((*f <|*) List.head l) :: List.map f (List.tail l))
Check.Quick mapRec

//alternative to using forAll
type EvenInt = EvenInt of int with
    static member op_Explicit(EvenInt i) = i

type ArbitraryModifiers =
    static member EvenInt() = 
        Arb.from<int> 
        |> Arb.filter (fun i -> i % 2 = 0) 
        |> Arb.convert EvenInt int
        
Arb.register<ArbitraryModifiers>() |> ignore

let ``generated even ints should be even`` (EvenInt i) = i % 2 = 0
Check.Quick ``generated even ints should be even``

let xUnitRunner =
    { new IRunner with
        member x.OnStartFixture t = ()
        member x.OnArguments (ntest,args, every) = ()
        member x.OnShrink(args, everyShrink) = ()
        member x.OnFinished(name,testResult) = 
            match testResult with 
            | TestResult.True _ -> () //Assert.True(true)
            | _ -> () //Assert.True(false, Runner.onFinishedToString name result) 
    }
   
let withxUnitConfig = { Config.Default with Runner = xUnitRunner }

type Foo = Foo of int
type Bar = Bar of string

let formatter (o:obj) =
    match o with
    | :? Foo as foo -> box "it's a foo"
    | :? Bar as bar -> box "it's a bar"
    | _ -> o

let formatterRunner =
    { new IRunner with
        member x.OnStartFixture t =
            printf "%s" (Runner.onStartFixtureToString t)
        member x.OnArguments (ntest,args, every) =
            printf "%s" (every ntest (args |> List.map formatter))
        member x.OnShrink(args, everyShrink) =
            printf "%s" (everyShrink (args |> List.map formatter))
        member x.OnFinished(name,testResult) = 
            let testResult' = match testResult with 
                                | TestResult.False (testData,origArgs,shrunkArgs,outCome,seed) -> 
                                    TestResult.False (testData,origArgs |> List.map formatter, shrunkArgs |> List.map formatter,outCome,seed)
                                | t -> t
            printf "%s" (Runner.onFinishedToString name testResult') 
    }
    
let testFormatter (foo:Foo) (bar:Bar) (i:int) = i < 10 //so it takes a while before the fail
Check.One({ Config.Quick with Runner = formatterRunner},testFormatter)


let (.=.) left right = left = right |@ sprintf "%A = %A" left right

let testCompare (i:int) (j:int) = 2*i+1  .=. 2*j-1
Check.Quick testCompare