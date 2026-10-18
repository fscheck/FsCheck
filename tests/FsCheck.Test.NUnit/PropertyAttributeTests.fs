module FsCheck.Test.NUnit.PropertyAttribute

open System.Reflection
open System.Threading.Tasks
open FsCheck
open NUnit.Framework
open NUnit.Framework.Internal
open FsCheck.FSharp
open FsCheck.NUnit

module ResultStateExceptionHandlingTest =
    [<Property>]
    let ``should pass when AssertPass called``() =
        NUnit.Framework.Assert.Pass()

    [<Property>]
    let ``should pass when AssertPass called inside async``() =
        async {
            NUnit.Framework.Assert.Pass()
        }

    [<Property>]
    let ``should pass when AssertPass called inside task``() =
        task {
            NUnit.Framework.Assert.Pass()
        }

module ResultOutputTests =
    [<Ignore("These should be run by the test below")>]
    module TestModule =
        type private T = T
        let ty = typeof<T>.DeclaringType

        type CustomType(value: int) =
            member _.Value = value

        type CustomTypeArb =
            static member CustomType() =
                ArbMap.defaults.ArbFor<int>()
                |> Arb.convert CustomType (fun (value: CustomType) -> value.Value)

        [<SetUp>]
        let setupFormatter() =
            TestContext.AddFormatter<CustomType>(fun value ->
                match value with
                | :? CustomType as custom -> $"CUSTOM:{custom.Value}"
                | _ -> value.ToString())

        [<Property>]
        let ``should show full result info on exn in test method`` () =
            do failwith "Some exception."

        [<Property>]
        let ``should show full result info on failing property in test method`` () =
            Prop.ofTestable false |> Prop.label "Some label."

        [<Property(MaxTest = 1, Arbitrary = [| typeof<CustomTypeArb> |])>]
        let ``should use NUnit's custom value formatter in failure output`` (value: CustomType) =
            Prop.ofTestable false

        [<Property>]
        let ``should show full result info on exn in Async<'Testable>-returning test method`` () =
            async {
                failwith "Some exception."
                return ()
            }

        [<Property>]
        let ``should show full result info on exn in Task-returning test method`` () : Task =
            task {
                failwith "Some exception."
                return ()
            }

        [<Property>]
        let ``should show full result info on failing property in Async<Property>-returning test method`` () =
            async {
                return Prop.ofTestable false |> Prop.label "Some label."
            }

        [<Property>]
        let ``should show full result info on failing property in Task<Property>-returning test method`` () =
            task {
                return Prop.ofTestable false |> Prop.label "Some label."
            }

    [<Property(MaxTest = 1)>]
    let ``should show full result info on failures in Async/Task-returning test methods`` () =
        let test = TestSuite TestModule.ty

        let testMethods =
            [|
                for meth in (TypeWrapper TestModule.ty).GetMethods (BindingFlags.Public ||| BindingFlags.Static) do
                    if not (Array.isEmpty (meth.GetCustomAttributes<PropertyAttribute> (``inherit``=true))) then
                        FsCheckTestMethod (meth, test)
            |]

        // SetUp methods are not run when invoking test methods directly, so register the formatter by hand.
        TestModule.setupFormatter ()
        let ctx = TestExecutionContext (TestExecutionContext.CurrentContext)
        let results = testMethods |> Array.map _.RunTest(ctx)

        let withoutFalsifiable =
            results
            |> Array.choose (fun result ->
                if result.Message.Contains "Falsifiable, after 1 test" then None
                else Some $"{result.Name}: {result.Message}\n{result.StackTrace}")

        withoutFalsifiable
        |> Array.isEmpty
        |> fun allFailuresAreReported ->
            let customFormatterOutputIsPresent =
                results
                |> Array.exists (fun result ->
                    result.Name.Contains "custom value formatter"
                    && result.Message.Contains "CUSTOM:")

            allFailuresAreReported && customFormatterOutputIsPresent
        |> Prop.label $"Expected falsification details and custom formatting; messages missing falsification details: '%A{withoutFalsifiable}'."
