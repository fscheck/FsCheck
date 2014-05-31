(*** hide ***)
#I "../../src/FsCheck/bin/Release"
#r "../../src/FsCheck.Xunit/bin/Release/FsCheck.Xunit.dll"
#r "../../packages/xunit.1.9.2/lib/net20/xunit.dll"

(**
# Quick Start

The fastest way to understand how FsCheck works is by writing some *properties* - FsCheck's terminology for a parametrized
test, or a generative test - and run them using the built-in test runner. Later on, we'll describe how they can be integrated
with existing test frameworks like NUnit, xUnit.NET or MsTest.

First install FsCheck, open an fsx file and start with:*)

#r "FsCheck"

open FsCheck

(**
## A Simple Example

A simple example of a property - a test with parameters - is written as a normal F# function that returns a bool: *)

let revRevIsOrig (xs:list<int>) = List.rev(List.rev xs) = xs
(** This property asserts that the reverse of the reverse of a list is the list itself. 
To check the property, we load this definition in F# interactive and then invoke *)

(*** define-output: revRevIsOrig ***)
Check.Quick revRevIsOrig

(*** include-output: revRevIsOrig ***)

(** When a property fails, FsCheck displays a counter-example. For example, if we define *)

let revIsOrig (xs:list<int>) = List.rev xs = xs

(** then checking it results in *)

(*** define-output: revIsOrig ***)
Check.Quick revIsOrig

(*** include-output: revIsOrig ***)

(** FsCheck also *shrinks* the counter example: it tries to find the minimal counter example that 
still fails the property. The counter example is indeed minimal: 
the list must have at least two different elements for the test to fail. FsCheck displays how many times it 
found a smaller (in some way) counter example and so proceeded to shrink further.

To learn more on how to write properties, see [Properties](Properties.html).

## What do I do if a test loops or encounters an error?

In this case we know that the property does not hold, but Check.Quick does not display the counter-example. 
There is another testing function provided for this situation. Repeat the test using 
<pre>Check.Verbose <property_name></pre>
which displays each test case before running the test: the last test case displayed is thus
the one in which the loop or error arises.

To learn more on how to run FsCheck tests see [Running Tests](RunningTests.html).

## Caveat

The property above (the reverse of the reverse of a list is the list itself) is not always correct. 
Consider a list of floats that contains `infinity`, or `nan` (not a number). Since `nan <> nan`, the reverse of 
the reverse of `[nan,nan]` is not actually equal to `[nan,nan]` if you use straightforward element by element 
comparison. FsCheck has a knack for finding this kind of specification problem. However, since this 
behavior is seldom what you want, FsCheck only generates values that are 'neatly' comparable when you leave 
the type polymorphic (currently, unit, bool, char and string values). To see this error, force 
FsCheck to generate lists of floats:*)

let revRevIsOrigFloat (xs:list<float>) = List.rev(List.rev xs) = xs

(***define-output:revFloat***)
Check.Quick revRevIsOrigFloat

(***include-output:revFloat***)

(**
## Using FsCheck with other testing frameworks

Once you have finished your initial exploration of FsCheck, you'll probably want to use it with your existing
unit test framework to augment unit tests or just to run the properties more easily.

### Straightforward integration with any unit test framework

As an example we'll use xUnit.NET, but the same strategy can be used with any test framework. Here is how to write 
the unit test above so it can be run from xUnit.NET:*)

open global.Xunit

[<Fact>]
let ``Reverse of reverse of a list is the original list``() =
  let revRevIsOrig (xs:list<int>) = List.rev(List.rev xs) = xs
  Check.QuickThrowOnFailure revRevIsOrig
  
(** For xUnit, the test looks like any normal test, and the QuickThrowOnFailure ensures that if the test fails,
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

To learn more on how to use this integration, see [Running Tests](RunningTests.html). *)
