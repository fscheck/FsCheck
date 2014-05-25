(*** hide ***)
#I "../FsCheck/bin/Debug"
#r "FsCheck"

open FsCheck


(**
# QuickStart

## A Simple Example

A simple example of a property definition is *)

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

(** FsCheck also shrinks the counter example, so that it is the minimal counter example that 
still fails the test case. In the example above, we see that the counter example is indeed minimal: 
the list must have at least two different elements. FsCheck also displays how many times it 
found a smaller (in some way) counter example and so proceeded to shrink further.

## Using FsCheck
To use FsCheck, you download the latest FsCheck source or binary, or use NuGet. Build and reference the assembly 
in any projects containing specifications or test data generators. You can then test 
properties by loading the module they are defined in into F# interactive, and calling 
<pre>Check.Quick <propertyName></pre>
or by running and writing a small console application that calls the Check function. Integration 
with unit test runners such as xUnit and NUnit is possible as well - see Usage Tips for an example.

## Grouping properties
Usually, you'll write more than one property to test. FsCheck allows you to group together properties as static members of a class: *)
type ListProperties =
  static member ``reverse of reverse is original`` xs = revRevIsOrig xs
  static member ``reverse is original`` xs = revIsOrig xs
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

(***include-output:ListProperties2***)

(**
## What do I do if a test loops or encounters an error?
In this case we know that the property does not hold, but Check.Quick does not display the counter-example. 
There is another testing function provided for this situation. Repeat the test using 
<pre>Check.Verbose <property_name></pre>
which displays each test case before running the test: the last test case displayed is thus
the one in which the loop or error arises. Check.VerboseAll can be used with types and modules
to check groups of properties verbosely.

## Caveat
The property above (the reverse of the reverse of a list is the list itself) is not always correct. 
Consider a list of floats that contains infinity, or nan (not a number). Since nan <> nan, the reverse of 
the reverse of {{[nan,nan]}} is not actually equal to {{[nan,nan]}} if you use straightforward element by element 
comparison. FsCheck has a knack for finding this kind of specification problem. However, since this 
behavior is seldom what you want, FsCheck only generates values that are 'neatly' comparable when you leave 
the type polymorphic (currently, unit, bool, char and string values). To see this error in action, force 
FsCheck to generate lists of floats:*)

let revRevIsOrigFloat (xs:list<float>) = List.rev(List.rev xs) = xs

(***define-output:revFloat***)
Check.Quick revRevIsOrigFloat

(***include-output:revFloat***)
