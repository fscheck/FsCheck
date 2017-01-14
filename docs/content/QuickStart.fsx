﻿(*** hide ***)
#I "../../src/FsCheck/bin/Release"
#r "../../src/FsCheck.Xunit/bin/Release/FsCheck.Xunit.dll"
#r "../../packages/xunit/lib/net20/xunit.dll"
#r "../../packages/Expecto/lib/net40/Expecto.dll"
#r "../../packages/Expecto/lib/net40/Expecto.FsCheck.dll"
(**
# Quick Start

The fastest way to understand how FsCheck works is by writing some *properties* - FsCheck's terminology for a parametrized
test, or a generative test - and run them using the built-in test runner. Later on, we'll describe how they can be integrated
with existing test frameworks like NUnit, xUnit.NET or MsTest.

First install FsCheck, open an fsx file and start with:*)

#r "FsCheck"

open FsCheck

(** In C#: To easily experiment, start a new console app to execute the snippets below (the output is written to console
by default). Alternatively, in LinqPad, reference FsCheck.dll and FSharp.Core.dll, open namespace FsCheck, change the language to "C# statements"
and you should be able to execute most of the snippets as well. 

## A Simple Example

A simple example of a property - a test with parameters - is written as a normal F# function that returns a bool: *)

let revRevIsOrig (xs:list<int>) = List.rev(List.rev xs) = xs
(** This property asserts that the reverse of the reverse of a list is the list itself. 
To check the property, we load this definition in F# interactive and then invoke *)

(*** define-output: revRevIsOrig ***)
Check.Quick revRevIsOrig

(*** include-output: revRevIsOrig ***)

(** In C#:

    [lang=csharp,file=../csharp/QuickStart.cs,key=revRevIsOrig]

When a property fails, FsCheck displays a counter-example. For example, if we define *)

(*** define-output: revIsOrig ***)
let revIsOrig (xs:list<int>) = List.rev xs = xs
Check.Quick revIsOrig

(*** include-output: revIsOrig ***)

(** In C#: 

    [lang=csharp,file=../csharp/QuickStart.cs,key=revIsOrig]

FsCheck also *shrinks* the counter example: it tries to find the minimal counter example that 
still fails the property. The counter example is indeed minimal: 
the list must have at least two different elements for the test to fail. FsCheck displays how many times it 
found a smaller (in some way) counter example and so proceeded to shrink further.

To learn more on how to write properties, see [Properties](Properties.html).

## What do I do if a test loops or encounters an error?

In this case we know that the property does not hold, but Check.Quick does not display the counter-example. 
There is another testing function provided for this situation. Repeat the test using 
<pre>Check.Verbose</pre> or in C# <pre>VerboseCheck()</pre>
which displays each test case before running the test: the last test case displayed is thus
the one in which the loop or error arises.

To learn more on how to run FsCheck tests see [Running Tests](RunningTests.html).

## FsCheck teaches us a lesson

The property above (the reverse of the reverse of a list is the list itself) is not always correct. 
Consider a list of floats that contains `NaN` (not a number). Since `NaN <> NaN`, the reverse of 
the reverse of `[NaN]` is not actually equal to `[NaN]`. FsCheck has a knack for finding this kind of specification problem. 
To see this error, ask FsCheck to generate lists of floats:*)

(***define-output:revFloat***)
let revRevIsOrigFloat (xs:list<float>) = List.rev(List.rev xs) = xs
Check.Quick revRevIsOrigFloat

(***include-output:revFloat***)

(** That said, the example in C# using floats actually works!

    [lang=csharp,file=../csharp/QuickStart.cs,key=revRevIsOrigFloat]

This is because SequenceEquals uses the default equality comparer under the hood, which uses `Double`'s `Equals` method, which
has a special provision for `NaN`, as you can see in the [reference source](http://referencesource.microsoft.com/#mscorlib/system/double.cs,152).
If we pass an `EqualityComparer` that uses `==` to `SequenceEquals`, the C# example also fails.

As this seemingly trivial example shows, FsCheck helps you discover interesting properties of your code - and so ultimately,
more bugs!

## Using FsCheck with other testing frameworks

Once you have finished your initial exploration of FsCheck, you'll probably want to use it with your existing
unit test framework to augment unit tests or just to run the properties more easily. Below, we'll show some
integration, but we leave it up to you to choose whichever suits your environment best.

### Integration with Expecto

Some testing frameworks like Expecto have an out-of-the-box integration with FsCheck. By using one of the runners that
support FsCheck out of the box, you gain access the the frameworks' reporting capabilities and integrations with IDEs
and build tooling.

Here's a sample:*)
open Expecto
open Expecto.ExpectoFsCheck

let config = { FsCheck.Config.Default with MaxTest = 10000 }

let properties =
  testList "FsCheck samples" [
    testProperty "Addition is commutative" <| fun a b ->
      a + b = b + a
      
    testProperty "Reverse of reverse of a list is the original list" <|
      fun (xs:list<int>) -> List.rev (List.rev xs) = xs

    // you can also override the FsCheck config
    testPropertyWithConfig config "Product is distributive over addition" <|
      fun a b c ->
        a * (b + c) = a * b + a * c
  ]

Tests.run defaultConfig properties
(*
### Integration with xUnit

Another frequently used runner is xUnit.NET. Here is how to write 
the unit test above so it can be run from xUnit.NET:*)

open global.Xunit

[<Fact>]
let ``Reverse of reverse of a list is the original list``() =
  let revRevIsOrig (xs:list<int>) = List.rev(List.rev xs) = xs
  Check.QuickThrowOnFailure revRevIsOrig
  
(**
    [lang=csharp,file=../csharp/QuickStart.cs,key=revRevIsOrigFact]

For xUnit, the test looks like any normal test, and the QuickThrowOnFailure ensures that if the test fails,
an exception with the necessary information is raised so xUnit knows the test failed. The output of the test is the same
as above.

### Using FsCheck with xUnit.NET using the plugin

xUnit.NET is "blessed" with an FsCheck plugin. To use it, install the FsCheck.Xunit NuGet package. The test above can now
be written more tersely as follows:*)

open FsCheck.Xunit

[<Property>]
let ``Reverse of reverse of a list is the original list ``(xs:list<int>) =
  List.rev(List.rev xs) = xs
  
(** xUnit now shows the test similarly to a regular test, and is able to run it directly.

To learn more on how to use this integration or integration with other frameworks like NUnit,
see [Running Tests](RunningTests.html). *)
