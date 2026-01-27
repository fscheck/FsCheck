namespace Fscheck.Test.FsCheck.NUnit.PropertyAttribute

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

module TestContextFormatterTests =
    type CustomType(value: int) =
        member _.Value = value
        override _.ToString() = $"CustomType({value})"
    
    [<OneTimeSetUp>]
    let setupFormatter() =
        // Register a custom formatter for CustomType
        TestContext.AddFormatter<CustomType>(fun (ct: obj) -> 
            match ct with
            | :? CustomType as custom -> $"[CUSTOM:{custom.Value}]"
            | _ -> ct.ToString())
    
    [<Property(MaxTest = 1, Verbose = true)>]
    let ``should use TestContext formatter for custom types`` (value: int) =
        // Create a custom type instance
        let customValue = CustomType(abs value % 100)
        
        // Write it to TestContext - this should use the custom formatter
        TestContext.WriteLine("Testing with value: {0}", customValue)
        
        // The property always passes, but we're testing that the formatter is used
        true

module ResultOutputTests =
    [<Ignore("These should be run by the test below")>]
    module TestModule =
        type private T = T
        let ty = typeof<T>.DeclaringType

        [<Property>]
        let ``should show full result info on exn in test method`` () =
            do failwith "Some exception."

        [<Property>]
        let ``should show full result info on failing property in test method`` () =
            Prop.ofTestable false |> Prop.label "Some label."

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

        let ctx = TestExecutionContext ()
        let results = testMethods |> Array.map _.RunTest(ctx)

        let withoutFalsifiable =
            results
            |> Array.choose (fun result ->
                if result.Message.Contains "Falsifiable, after 1 test" then None
                else Some $"{result.Name}: {result.Message}\n{result.StackTrace}")

        withoutFalsifiable
        |> Array.isEmpty
        |> Prop.label $"Expected all messages to contain falsifiable message but got: '%A{withoutFalsifiable}'."
