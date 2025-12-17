open FsCheck.FSharp


(*** hide ***)
#I @"../src/FsCheck/bin/Release/netstandard2.0"
#I @"../src/FsCheck.Xunit/bin/Release/netstandard2.0"
#I @"../src/FsCheck.NUnit/bin/Release/net6.0"
#r @"nuget: xunit.core"
#r @"nuget: NUnit"
#r "FsCheck"
#r "FsCheck.Xunit"
#r "FsCheck.NUnit"

open FsCheck
open System
open FsCheck.FSharp

(**
# Running tests

This section describes the various ways in which you can run FsCheck tests:

* FsCheck has a built-in test runner that is easy to invoke from F#/C# Interactive, commandline program or any test framework. 
It writes the result of tests to standard output, and you can configure the FsCheck runner to throw an exception on test failure
to signal the failure to whichever test runner you use.

* FsCheck.Xunit integrates FsCheck with xUnit.net to allow you to specify properties in a terse way. Tests written
this way look like native xUnit.net tests, except they can take arguments.

* FsCheck.NUnit integrates FsCheck with NUnit to allow you to specify properties in a terse way. Tests written
this way look like native NUnit tests, except they can take arguments.

* FsCheck allows you to register an IRunner implementation that it calls on each outcome and each individual test it runs. This allows
a tighter integration with a specific test framework, and offers more control over printing of results.

## Using the built-in test runner

### Running a single property from an fsx/csx file or a command line runner

From F# the preferred way of running a property is to use the methods on `Check`; for C# the preferred way is to use the extension
methods on the `Property` type.

`Check.One` or `<property.Check>` runs the tests for a single property. It takes as argument an instance of the `Config` type
or `Configuration` type respectively, which allows you to configure many aspects of how the FsCheck test runner works, 
among others the size of the test data and the random seed used (so you can reproduce a run). The `Config` type is an F# 
record type so is the preferred way to use from F#; for other languages `Configuration` is a mutable
version of it so is easier to use.

Most of the methods on `Check` and the extension methods like `QuickCheck()` are a short-hand for running a test with a particular configuration. For example, `Check.Quick` 
is equivalent to `Check.One(Config.Quick, <property>)`; respectively `<property>.QuickCheck()` is a shorthand for `<property>.Check(Configuration.Quick)`.

Take note of `Config(uration).Verbose` and `Check.Verbose`/`VerboseCheck`. These will print the arguments for each test, and are useful for example if
your test loops infinitely on some inputs.

Also take note of `Check.QuickThrowOnFailure`/`QuickCheckThrowOnFailure()` and `Check.VerboseThrowOnFailure`/`VerboseThrowOnFailure()`. 
These are intended to be used from unit tests executed through an existing test runner. They'll call the FsCheck runner 
and configure it such that is throws on failure; in all frameworks we are aware of this signals that the test has failed 
and so is an easy way to integrate FsCheck tests with unit tests you may already have.

Here is an example of how to run a test with a similar configuration to Quick, but that runs 1000 tests and does not print to
the output on success:*)

Check.One(Config.Quick.WithMaxTest(1000).WithQuietOnSuccess(true), fun _ -> true)

(**
    [lang=csharp, file=../examples/CSharp.DocSnippets/RunningTests.cs,key=configuration]

### Running many properties at once with Check.All

Usually, you'll write more than one property to test. FsCheck allows you to group together properties as static members of a class: *)
type ListProperties =
  static member ``reverse of reverse is original`` (xs:list<int>) = List.rev(List.rev xs) = xs
  static member ``reverse is original`` (xs:list<int>) = List.rev xs = xs
(**These can be checked at once using:*)

(***define-output:ListProperties***)
Check.QuickAll<ListProperties>()

(**FsCheck now also prints the name of each test:*)

(***include-output:ListProperties***)

(**Since all top level functions of a a module are also compiled as static member of a class with the name of the module, 
you can also use Check.QuickAll to test all the top level functions in a certain module. 
However, the type of a module is not directly accessible via F#, so you can use the following trick:*)

(***define-output:ListProperties2***)
Check.QuickAll typeof<ListProperties>.DeclaringType

(**
Notice also the counterpart of `Check.Verbose`: `Check.VerboseAll`.

### Running tests using only modules

Arbitrary instances are given as static members of classes, and properties can be grouped together 
as static members of classes. Top level let functions are compiled as static member of their 
enclosing module (which is compiled as a class), and so you can simply define your properties and generators as 
top level let-bound functions, and then register all generators and and all properties at once using the following trick:*)

(***define-output:Marker***)
let myprop (i:int) = i >= 0
let mygen = ArbMap.defaults |> ArbMap.arbitrary<int> |> Arb.mapFilter (fun i -> Math.Abs i) (fun i -> i >= 0)
let helper = "a string"
let private helper' = true

type Marker = class end
let config = Config.QuickThrowOnFailure.WithArbitrary([typeof<Marker>.DeclaringType])
Check.All(config, typeof<Marker>.DeclaringType)

(***include-output:Marker***)

(**
The Marker type is just any type defined in the module, to be able to get to the module's Type. F# offers no way 
to get to a module's Type directly.

FsCheck determines the intent of the function based on its return type:

* Properties: public functions that return unit, bool, Property or function of any arguments to those types 
or Lazy value of any of those types. So `myprop` is the only property that is run; `helper'` also returns bool but is private.
* Arbitrary instances: return Arbitrary<_>

All other functions are respectfully ignored. If you have top level functions that return types that FsCheck will 
do something with, but do not want them checked or registered, just make them private. FsCheck will ignore those functions.

## Using FsCheck.Xunit

To use the integration install the FsCheck.Xunit nuget package. Then: *)

open FsCheck
open FsCheck.Xunit

(**
You can now attribute tests with `PropertyAttribute` (a subclass of xUnit.net's `FactAttribute`). Unlike xUnit.net's facts, these 
methods can take arguments and should return a property. FsCheck will be used to generate and shrink the arguments based on the
type and the currently registered generators. 

An FsCheck test fails from xUnit.net's perspective if it finds a counter-example, or if the arguments are exhausted. It
passes when FsCheck can execute 100 tests (or whatever the configured number of tests is) succesfully.

The `PropertyAttribute` allows you to customize how FsCheck will run for that
method, similar to how you would use the `Config` type otherwise. For example:*)

[<Property>]
let ``abs(v) % k equals abs(v % k)`` v (NonZeroInt k) = 
    (abs v) % k = abs(v % k)

(**
Likely one of the most useful configuration options of `PropertyAttribute` is the ability to register or override an `Arbitrary`
instance just for that test method. You can also use the `PropertiesAttribute` (note the plural form) to set custom configuration
options per class, per module, or per assembly. For example:*)

type Positive =
    static member Double() =
        ArbMap.defaults
        |> ArbMap.arbitrary<float>
        |> Arb.mapFilter abs (fun t -> t > 0.0)

type Negative =
    static member Double() =
        ArbMap.defaults
        |> ArbMap.arbitrary<float>
        |> Arb.mapFilter (abs >> ((-) 0.0)) (fun t -> t < 0.0)

type Zero =
    static member Double() =
        0.0
        |> Gen.constant
        |> Arb.fromGen

[<assembly: Properties( Arbitrary = [| typeof<Zero> |] )>] do()

module ModuleWithoutProperties =

    [<Property>]
    let ``should use Arb instances from assembly``(underTest:float) =
        underTest = 0.0

    [<Property( Arbitrary=[| typeof<Positive> |] )>]
    let ``should use Arb instance on method``(underTest:float) =
        underTest > 0.0

[<Properties( Arbitrary=[| typeof<Negative> |] )>]
module ModuleWithProperties =

    [<Property>]
    let ``should use Arb instances from enclosing module``(underTest:float) =
        underTest < 0.0

    [<Property( Arbitrary=[| typeof<Positive> |] )>]
    let ``should use Arb instance on method``(underTest:float) =
        underTest > 0.0

(**
Using `PropertiesAttribute` and `PropertyAttribute` you can set any configuration. For example in following module:

* property 1 would use default config + overriden MaxTest = 10 and EndSize = 10 from `Properties` attribute
* property 2 would use default config + overriden EndSize = 10 from `Properties` attribute and MaxTest = 500 from `Property` attribute
* property 3 would use default config + overriden MaxTest = 10 and EndSize = 10 from `Properties` attribute and Replay = "123,456" from `Property` attribute 
*)

[<Properties(MaxTest = 10, EndSize = 10)>] 
module Properties =

    [<Property>]
    let ``property 1`` input =
        true

    [<Property(MaxTest = 500)>]
    let ``property 2`` input =
        true

    [<Property(Replay = "123,456")>]
    let ``property 3`` input =
        true

(**
### Using FsCheck.Xunit with TestDriven.Net

[TestDriven.Net](http://testdriven.net) is a Visual Studio add-in that enables you to easily run a variety of tests
while working with code in the IDE. Out of the box, TestDriven.Net can run tests written with FsCheck.Xunit.

However, the user experience may, by default, be slightly less than optimal. TestDriven.Net outputs the state of the
test run in an unobtrusive manner, but if test failures occur, it outputs the result of the failing tests to Visual
Studio's Output window. If you have Visual Studio configured in such a way that the Output window only appears if there
actually *is* any output, you may be used to interpret the appearance of the Output window as a test failure.

Since the Output window also appears if anything is written to the console in general, this can produce false
positives. If you're accustomed to interpret the appearance of the Output window as a sign of a test failure, it can be
a little jarring that FsCheck by default always reports a bit of output on success.

If, for that, or other reasons, you want to disable output on success, you can do so:
*)

[<Property(QuietOnSuccess = true)>]
let ``abs(v) % k equals abs(v % k) `` v (NonZeroInt k) = 
    (abs v) % k = abs(v % k)

(**
Setting `QuietOnSuccess = true` only suppresses the output in case of success; in the case of test failures, output
appears as normal.

### Capturing output when using `FactAttribute`

xUnit 2 doesn't capture messages written to the console but instead provides `ITestOutputHelper` to [capture output](https://xunit.github.io/docs/capturing-output.html).
`ITestOutputHelper` has a single method `WriteLine` and xUnit will automatically pass it in as a constructor argument.
FsCheck.Xunit provides overloads for `Property.QuickCheck`, `Property.QuickCheckThrowOnFailure`, `Property.VerboseCheck` and `Property.VerboseCheckThrowOnFailure`
that you can pass an `ITestOutputHelper` so that xUnit captures FsCheck messages:

```
using System;
using FsCheck;
using FsCheck.Xunit;
using Xunit;
using Xunit.Abstractions;

public class Test
{
    private readonly ITestOutputHelper _TestOutputHelper;
    public Test(ITestOutputHelper testOutputHelper)
    {
        _TestOutputHelper = testOutputHelper;
    }

    [Fact]
    public void Test1()
    {
        Prop
            .ForAll(...)
            .VerboseCheckThrowOnFailure(_TestOutputHelper);
    }
}
```
*)

(**
## Using FsCheck.NUnit

To use the integration with NUnit 3 install the FsCheck.NUnit nuget package.
Make sure your project(s) has target .NET `4.5` or greater.
Then open FsCheck.NUnit.

You can now attribute tests with `PropertyAttribute` (a subclass of NUnit's `TestAttribute`). Unlike NUnit tests, these 
methods can take arguments and should return a property. FsCheck will be used to generate and shrink the arguments based on the
type and the currently registered generators. 

An FsCheck test fails from NUnit's perspective if it finds a counter-example, or if the arguments are exhausted. It
passes when FsCheck can execute 100 tests (or whatever the configured number of tests is) succesfully.

The `PropertyAttribute` allows you to customize how FsCheck will run for that
method, similar to how you would use the `Config` type otherwise.
*)

open FsCheck.NUnit

[<Property>]
let ``Reverse of reverse of a list is the original list ``(xs:list<int>) =
  List.rev(List.rev xs) = xs

(**
Note: the NUnit integration doesn't have the ability, like FsCheck.Xunit, to override `Arbitrary` instances on a per class
or per module basis. Otherwise, it is very similar.

## Implementing IRunner 

### Example 1: to integrate FsCheck with other unit testing frameworks

The `Config` type that can be passed to the `Check.One` or `Check.All` methods takes an `IRunner` as argument. This i
nterface has the following methods:

* `OnStartFixture` is called when FsCheck is testing all the methods on a type, before starting any tests.
* `OnArguments` is called after every test, passing the implementation the test number, the arguments and the every function. 
* `OnShrink` is called at every succesful shrink.
* `OnFinished` is called with the name of the test and the outcome of the overall test run. This is used in the example below 
to call Assert statements from a particular unit testing framework - allowing FsCheck to integrate easily. You can leverage 
another unit testing framework's ability to setup and tear down tests, have a nice graphical runner etc.*)

let testRunner =
  { new IRunner with
      member __.OnStartFixture t = ()
      member __.OnArguments (ntest,args, every) = ()
      member __.OnShrink(args, everyShrink) = ()
      member __.OnFinished(name,testResult) = 
          match testResult with 
          | TestResult.Passed _ -> () //let the test runner know that the test passed
          | _ -> () // test failed, or other problem. Notify test runner. Runner.onFinishedToString name testResult
  }
   
let withxUnitConfig = Config.Default.WithRunner(testRunner)

(**
### Example 2: to customize printing of generated arguments

By default, FsCheck prints generated arguments using `sprintf "%A"`, or structured formatting. This usually does what you expect, 
i.e. for primitive types the value, for objects the ToString override and so on. If it does not (A motivating case is 
testing with COM objects - overriding ToString is not an option and structured formatting does not do anything useful with it), 
you can use the `label` combinator to solve this on a per property basis, but a more structured solution can be achieved by 
implementing `IRunner`. For example:*)
    
let formatterRunner formatter =
  { new IRunner with
      member x.OnStartFixture t =
          printf "%s" (Runner.onStartFixtureToString t)
      member x.OnArguments (ntest,args, every) =
          printf "%s" (every ntest (args |> List.map formatter))
      member x.OnShrink(args, everyShrink) =
          printf "%s" (everyShrink (args |> List.map formatter))
      member x.OnFinished(name,testResult) = 
          let testResult' = match testResult with 
                              | TestResult.Failed (testData,origArgs,shrunkArgs,outCome,seed, seed2, size) -> 
                                  TestResult.Failed (testData,origArgs |> List.map formatter, shrunkArgs |> List.map formatter,outCome,seed, seed2, size)
                              | t -> t
          printf "%s" (Runner.onFinishedToString name testResult') 
  }

(**
### Using FsCheck.NUnit with TestDriven.Net

[TestDriven.Net](http://testdriven.net) is a Visual Studio add-in that enables you to easily run a variety of tests
while working with code in the IDE. Out of the box, TestDriven.Net can run tests written with FsCheck.NUnit.

However, the user experience may, by default, be slightly less than optimal. TestDriven.Net outputs the state of the
test run in an unobtrusive manner, but if test failures occur, it outputs the result of the failing tests to Visual
Studio's Output window. If you have Visual Studio configured in such a way that the Output window only appears if there
actually *is* any output, you may be used to interpret the appearance of the Output window as a test failure.

Since the Output window also appears if anything is written to the console in general, this can produce false
positives. If you're accustomed to interpret the appearance of the Output window as a sign of a test failure, it can be
a little jarring that FsCheck by default always reports a bit of output on success.

If, for that, or other reasons, you want to disable output on success, you can do so:
*)

[<Property(QuietOnSuccess = true)>]
let revUnit (x:char) = 
    List.rev [x] = [x]

(**
Setting `QuietOnSuccess = true` only suppresses the output in case of success; in the case of test failures, output
appears as normal.
*)
