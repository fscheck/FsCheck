
namespace FsCheck.Xunit

open System

open Xunit
open Xunit.Sdk
open FsCheck

type PropertyFailedException(testResult:FsCheck.TestResult) = 
    inherit AssertException(sprintf "%s%s" Environment.NewLine (Runner.onFinishedToString "" testResult), "sorry no stacktrace")

//can not be an anonymous type because of let mutable.
type private XunitRunner() =
    let mutable result = None
    member x.Result = result.Value
    interface IRunner with
        override x.OnStartFixture t = ()
        override x.OnArguments (ntest,args, every) = 
            printf "%s" (every ntest args)
        override x.OnShrink(args, everyShrink) = 
            printf "%s" (everyShrink args)
        override x.OnFinished(name,testResult) = 
            result <- Some testResult

[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
type ArbitraryAttribute(types:Type[]) = 
    inherit Attribute()
    new(typ:Type) = ArbitraryAttribute([|typ|])
    member x.Arbitrary = types

[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type PropertyAttribute() =
    inherit FactAttribute()
    let mutable maxTest = Config.Default.MaxTest
    let mutable maxFail = Config.Default.MaxFail 
    let mutable replay = Config.Default.Replay
    let mutable startSize = Config.Default.StartSize
    let mutable endSize = Config.Default.EndSize
    let mutable verbose = false
    let mutable arbitrary = Config.Default.Arbitrary |> List.toArray

    member x.Replay with get() = match replay with None -> String.Empty | Some (Random.StdGen (x,y)) -> sprintf "%A" (x,y)
                    and set(v:string) = 
                        //if someone sets this, we want it to throw if it fails
                        let split = v.Trim('(',')').Split([|","|], StringSplitOptions.RemoveEmptyEntries)
                        let elem1 = Int32.Parse(split.[0])
                        let elem2 = Int32.Parse(split.[1])
                        replay <- Some <| Random.StdGen (elem1,elem2)
    member internal x.ReplayStdGen = replay
    member x.MaxTest with get() = maxTest and set(v) = maxTest <- v
    member x.MaxFail with get() = maxFail and set(v) = maxFail <- v
    member x.StartSize with get() = startSize and set(v) = startSize <- v
    member x.EndSize with get() = endSize and set(v) = endSize <- v
    member x.Verbose with get() = verbose and set(v) = verbose <- v
    member x.Arbitrary with get() = arbitrary and set(v) = arbitrary <- v

    override this.EnumerateTestCommands(methodInfo:IMethodInfo) :seq<ITestCommand> = 
        { new TestCommand(methodInfo, null, 0) with
            override x.Execute(testClass:obj) : MethodResult = 
                let xunitRunner = XunitRunner()
                let arbitraries = 
                    methodInfo.Class.Type 
                    |> Seq.unfold (fun t -> if t <> null then Some(t,t.DeclaringType) else None)
                    |> Seq.map (fun t -> t.GetCustomAttributes(typeof<ArbitraryAttribute>, true))
                    |> Seq.filter (fun attr -> attr.Length = 1)
                    |> Seq.collect (fun attr -> (attr.[0] :?> ArbitraryAttribute).Arbitrary)
                    |> Seq.append this.Arbitrary
                    |> Seq.toList
                let config = 
                    {Config.Default with
                        Replay = this.ReplayStdGen
                        MaxTest = this.MaxTest
                        MaxFail = this.MaxFail
                        StartSize = this.StartSize
                        EndSize = this.EndSize
                        Every = if this.Verbose then Config.Verbose.Every else Config.Quick.Every
                        EveryShrink = if this.Verbose then Config.Verbose.EveryShrink else Config.Quick.EveryShrink
                        Arbitrary = arbitraries
                        Runner = xunitRunner
                    }
                Check.Method(config, methodInfo.MethodInfo,?target=if testClass <> null then Some testClass else None)
                match xunitRunner.Result with
                | TestResult.True _ -> 
                    printf "%s%s" Environment.NewLine (Runner.onFinishedToString "" xunitRunner.Result)
                    upcast new PassedResult(methodInfo,this.DisplayName)
                | Exhausted testdata -> 
                    upcast new FailedResult(methodInfo,PropertyFailedException(xunitRunner.Result), this.DisplayName)
                | TestResult.False (testdata, originalArgs, shrunkArgs, outcome, seed)  -> 
                    upcast new FailedResult(methodInfo, PropertyFailedException(xunitRunner.Result), this.DisplayName)
        } :> ITestCommand
        |> Seq.singleton