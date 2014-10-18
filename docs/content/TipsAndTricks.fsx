(*** hide ***)
#I @"../../src/FsCheck/bin/Release"
#r @"FsCheck"
open FsCheck
open System

(**
# Tips and Tricks
    
## Properties of functions

Since FsCheck can generate random function values, it can check properties of 
functions. For example, we can check associativity of function composition as follows:*)

(***define-output:associativity***)
let associativity (x:int) (f:int->float,g:float->char,h:char->int) = ((f >> g) >> h) x = (f >> (g >> h)) x
Check.Quick associativity

(***include-output:associativity***)

(**
We can generate functions Tree -> _anything_. If a counter-example is found, function values will be displayed as <func>.

However, FsCheck can show you the generated function in more detail, with the Function type. 
Then FsCheck can even shrink your function. For example:*)

(***define-output:mapRec***)
let mapRec (F (_,f)) (l:list<int>) =
  not l.IsEmpty ==>
      lazy (List.map f l = ((*f <|*) List.head l) :: List.map f (List.tail l))
Check.Quick mapRec

(***include-output:mapRec***)

(**
The type `Function<'a,'b>` records a map of all the arguments it was called with, and the result it produced. 
In your properties, you can extract the actual function by pattern matching as in the example. 
Function is used to print the function, and also to shrink it.
    
## Use pattern matching instead of forAll to use custom generators

To define a generator that generates a subset of the normal range of values for an existing type,
say all the even ints, it makes properties more readable if you define a single-case union
case, and register a generator for the new type:
*)

(***define-output:EvenInt***)
type EvenInt = EvenInt of int with
  static member op_Explicit(EvenInt i) = i

type ArbitraryModifiers =
    static member EvenInt() = 
        Arb.from<int> 
        |> Arb.filter (fun i -> i % 2 = 0) 
        |> Arb.convert EvenInt int
        
Arb.register<ArbitraryModifiers>()

let ``generated even ints should be even`` (EvenInt i) = i % 2 = 0
Check.Quick ``generated even ints should be even``

(***include-output:EvenInt***)

(**
It's now easy to define custom shrink functions as well.

FsCheck uses this pattern frequently, e.g. `NonNegativeInt`, `PositiveInt`, `StringWithoutNullChars` etc. See the
default Arbitrary instances on the `Arb.Default` type.

Also, for these kinds of generators, the `Arb.filter`, `Arb.convert` and `Arb.mapFilter` functions will come in handy.
  
## An equality comparison that prints the left and right sides of the equality

Properties commonly check for equality. If a test case fails, FsCheck prints the counterexample, but 
sometimes it is useful to print the left and right side of the comparison, especially if you 
do some complicated calculations with the generated arguments first. To make this easier, you can 
define your own labelling equality combinator:*)

(***define-output:testCompare***)
let (.=.) left right = left = right |@ sprintf "%A = %A" left right

let testCompare (i:int) (j:int) = 2*i+1  .=. 2*j-1
Check.Quick testCompare

(***include-output:testCompare***)

(**
Of course, you can do this for any operator or function that you often use.
    
## Some ways to run FsCheck tests

* By adding properties and generators to an fsx file in your project. It's easy to execute, just press 
ctrl-a and alt-enter, and the results are displayed in F# Interactive. Be careful when referencing dlls 
that are built in your solution; Versions of F# Interactive earlier than 3.1.2 will lock those for the remainder of the session, 
and you won't be able to build unitl you quit the session. One solution is to include the source files 
instead of the dlls, but that makes the process slower. Useful for smaller projects. Difficult to debug though.
* By making a separate console application. Easy to debug, and no annoying locks on assemblies. Your best option 
if you use only FsCheck for testing and your properties span multiple assemblies.
* By using another unit testing framework. Useful if you have a mixed FsCheck/unit testing approach 
(some things are easier to check using unit tests, and vice versa), and you like a graphical runner. 
Depending on what unit testing framework you use, you may get good integration with Visual Studio for free. Also have a look
at some of the existing integrations with test runners like Xunit.NET, NUnit, Fuchu.
*)
