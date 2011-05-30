
namespace FsCheck

open System
open Xunit
open Xunit.Sdk


type PropertyFailedException(testResult:FsCheck.TestResult) = 
    inherit AssertException(sprintf "%s%s" Environment.NewLine (Runner.onFinishedToString "" testResult))
      

type internal PropertyTestCommand(methodInfo:IMethodInfo) =
    inherit TestCommand(methodInfo, null, 0)
    let mutable result = None
    let xunitRunner =
        { new IRunner with
            member x.OnStartFixture t = ()
                //printf "%s" (onStartFixtureToString t)
            member x.OnArguments (ntest,args, every) = ()
                //printf "%s" (every ntest args)
            member x.OnShrink(args, everyShrink) = ()
                //printf "%s" (everyShrink args)
            member x.OnFinished(name,testResult) = 
                result <- Some testResult
                //printf "%s" (onFinishedToString name testResult)
        }


    override this.Execute(testClass:obj) : MethodResult = 
        Check.Method( { Config.Default with Runner = xunitRunner }, methodInfo.MethodInfo)
        match result.Value with
        | TestResult.True _ -> 
            upcast new PassedResult(methodInfo,this.DisplayName)
        | Exhausted testdata as testResult -> 
            upcast new FailedResult(methodInfo,PropertyFailedException(testResult), this.DisplayName)
        | TestResult.False (testdata, originalArgs, shrunkArgs, outcome, seed) as testResult -> 
            upcast new FailedResult(methodInfo, PropertyFailedException(testResult), this.DisplayName)



[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type PropertyAttribute() =
    inherit FactAttribute()
    override x.EnumerateTestCommands(``method``:IMethodInfo) :seq<ITestCommand> = 
        Seq.singleton <| upcast new PropertyTestCommand(``method``)