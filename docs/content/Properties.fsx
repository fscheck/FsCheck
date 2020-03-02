(*** hide ***)
#I "../../src/FsCheck/bin/Release/netstandard2.0"
#r "FsCheck"

open FsCheck
open System

(**
# Properties

Properties are expressed as F# function definitions or C# lambdas or methods. 
Properties are universally quantified over their parameters, so *)

let revRevIsOrig (xs:list<int>) = List.rev(List.rev xs) = xs

(**
    [lang=csharp,file=../csharp/Properties.cs,key=revRevIsOrig]

means that the equality holds for all lists xs.

Properties must not have generic types - because there can be so many different
kinds of constraints on generic types, some of which may not even be visible from the
type signature, we currently think allowing FsCheck to generate a generic type is not worth the added complexity. 
It's very simple to fix any types anyway simply by adding some type annotations.

FsCheck can check properties of various forms - these forms are called testable, 
and are indicated in the API by a generic type called `'Testable`. A `'Testable` may 
be a function of any number of parameters that returns bool or unit. In the latter case, 
 a test passes if it does not throw. The entry point to create properties is the Prop module.

 Like all of FsCheck's API, there are C# counterparts for all of the F# methods described.
    
## Conditional Properties

Properties may take the form `<condition> ==> <property>`

For example,*)

(***hide***)
let rec ordered xs = 
  match xs with
  | [] -> true
  | [x] -> true
  | x::y::ys ->  (x <= y) && ordered (y::ys)
let rec insert x xs = 
  match xs with
  | [] -> [x]
  | c::cs -> if x <= c then x::xs else c::(insert x cs)

(***define-output:insertKeepsOrder***)
let insertKeepsOrder (x:int) xs = ordered xs ==> ordered (insert x xs)
Check.Quick insertKeepsOrder

(**
    [lang=csharp,file=../csharp/Properties.cs,key=insertKeepsOrder]    *)

(***include-output:insertKeepsOrder***)

(**
Such a property holds if the property after `==>` holds whenever the condition does.

Testing discards test cases which do not satisfy the condition. Test case generation 
continues until 100 cases which do satisfy the condition have been found, or until 
an overall limit on the number of test cases is reached (to avoid looping if the condition 
never holds). In this case a message such as "Arguments exhausted after 97 tests."
indicates that 97 test cases satisfying the condition were found, and that the property held in those 97 cases.

Notice that in this case the generated values had to be restricted to int. This is because the generated 
values need to be comparable, but this is not reflected in the types. Therefore, without the explicit 
restriction, FsCheck could generate lists containing different types (subtypes of objects), and these are not mutually comparable.
    
## Lazy Properties

Since F# has eager evaluation by default, the above property does more work than necessary: 
it evaluates the property at the right of the condition no matter what the 
outcome of the condition on the left. While only a performance consideration in the above 
example, this may limit the expressiveness of properties - consider:*)

(***define-output: eager***)
let tooEager a = a <> 0 ==> (1/a = 1/a)
Check.Quick tooEager

(***include-output: eager***)

(**
Non-strict evaluation is needed here to make sure the propery is checked correctly:*)

(***define-output: lazy***)
let moreLazy a = a <> 0 ==> (lazy (1/a = 1/a))
Check.Quick moreLazy

(**
    [lang=csharp,file=../csharp/Properties.cs,key=lazy] *)

(***include-output: lazy***)

(**
## Quantified Properties

Properties may take the form `forAll <arbitrary>  (fun <args> -> <property>)`.

For example, *)

(***define-output:insertWithArb***)
let orderedList = Arb.from<list<int>> |> Arb.mapFilter List.sort ordered
let insertWithArb x = Prop.forAll orderedList (fun xs -> ordered(insert x xs))
Check.Quick insertWithArb

(**
    [lang=csharp,file=../csharp/Properties.cs,key=insertWithArb] *)

(***include-output:insertWithArb***)

(**
The first argument of forAll is an IArbitrary instance. Such an instance 
encapsulates a test data generator and a shrinker (more on that in [Test Data](TestData.html)).
By supplying a custom generator, instead of using the default generator 
for that type, it is possible to control the distribution of test data. In 
the example, by supplying a custom generator for ordered lists, rather than 
filtering out test cases which are not ordered, we guarantee that 100 test 
cases can be generated without reaching the overall limit on test cases. 
Combinators for defining generators are described in [Test Data](TestData.html).
    
## Expecting exceptions

You may want to test that a function or method throws an exception under certain circumstances. 
Use `throws<'e :> exn,'a> Lazy<'a>` to achieve this. For example:*)

(***define-output: expectDivideByZero***)
let expectDivideByZero() = Prop.throws<DivideByZeroException,_> (lazy (raise <| DivideByZeroException()))
Check.Quick expectDivideByZero

(***include-output: expectDivideByZero***)
  
(**
This functionality is not available in the C# API.

## Timed Properties

Properties may take the form `within <timeout in ms> <Lazy<property>>`

For example,*)

let timesOut (a:int) = 
    lazy
        if a>10 then
            do Threading.Thread.Sleep(3000)
            true
        else 
            true
    |> Prop.within 1000

(**
The first argument is the time the lazy property may run. If it runs longer, 
FsCheck considers the test as failed. Otherwise, the outcome of the lazy property is 
the outcome of within. Note that, although within attempts to cancel the thread in which 
the property is executed, that may not succeed, and so the thread may actually continue to run until the process ends.

This functionality is not available in the C# API.
    
## Observing Test Case Distribution

It is important to be aware of the distribution of test cases: if test data is not well 
distributed then conclusions drawn from the test results may be invalid. In particular, 
the `==>` operator can skew the distribution of test data badly, since only test data which 
satisfies the given condition is used.

FsCheck provides several ways to observe the distribution of test data. Code for 
making observations is incorporated into the statement of properties, each time 
the property is actually tested the observation is made, and the collected observations 
are then summarized when testing is complete.

### Counting Trivial Cases

A property may take the form `trivial <condition> <property>`

For example,*)

(***define-output:insertTrivial***)
let insertTrivial (x:int) xs = 
  ordered xs ==> (ordered (insert x xs))
  |> Prop.trivial (List.length xs = 0)
Check.Quick insertTrivial

(**
    [lang=csharp,file=../csharp/Properties.cs,key=insertTrivial]

Test cases for which the condition is true are classified as trivial, and the proportion of 
trivial test cases in the total is reported:*)

(***include-output:insertTrivial***)

(**
### Classifying Test Cases

A property may take the form `classify <condition> <string> <property>`

For example,*)

(***define-output:insertClassify***)
let insertClassify (x:int) xs = 
  ordered xs ==> (ordered (insert x xs))
  |> Prop.classify (ordered (x::xs)) "at-head"
  |> Prop.classify (ordered (xs @ [x])) "at-tail"
Check.Quick insertClassify

(**
    [lang=csharp,file=../csharp/Properties.cs,key=insertClassify]

Test cases satisfying the condition are assigned the classification given, and the distribution of 
classifications is reported after testing:*)

(***include-output:insertClassify***)

(**
Note that a test case may fall into more than one classification.

### Collecting Data Values

A property may take the form `collect <expression> <property>`

For example,*)

(***define-output: insertCollect***)
let insertCollect (x:int) xs = 
  ordered xs ==> (ordered (insert x xs))
      |> Prop.collect (List.length xs)
Check.Quick insertCollect

(**
    [lang=csharp,file=../csharp/Properties.cs,key=insertCollect]

The argument of collect is evaluated in each test case, and the distribution of 
values is reported. The type of this argument is printed using `sprintf "%A"`:*)

(***include-output: insertCollect***)

(**  
### Combining Observations

The observations described here may be combined in any way. All the observations 
of each test case are combined, and the distribution of these combinations is 
reported. For example:*)

(***define-output:insertCombined***)
let insertCombined (x:int) xs = 
    ordered xs ==> (ordered (insert x xs))
    |> Prop.classify (ordered (x::xs)) "at-head"
    |> Prop.classify (ordered (xs @ [x])) "at-tail"
    |> Prop.collect (List.length xs)
Check.Quick insertCombined

(**
    [lang=csharp,file=../csharp/Properties.cs,key=insertCombined]*)

(***include-output:insertCombined***)

(**
## And, Or and Labels

Properties may take the form

* `<property> .&. <property>` succeeds if both succeed, fails if one of the properties fails, and is rejected when both are rejected.
* `<property> .|. <property>`succeeds if either property succeeds, fails if both properties fail, and is rejected when both are rejected.

The `.&.` combinator is most commonly used to write complex properties which share a generator. 
In that case, it might be difficult upon failure to know excactly which sub-property has caused the failure. 
That's why you can label sub-properties, and FsCheck shows the labels of the failed subproperties when 
it finds a counter-example. This takes the form: `<string> @| <property>` or `<property> |@ <string>`.

For example,*)

(***define-output:complex***)
let complex (m: int) (n: int) =
  let res = n + m
  (res >= m)    |@ "result > #1" .&.
  (res >= n)    |@ "result > #2" .&. 
  (res < m + n) |@ "result not sum"
Check.Quick complex

(**
    [lang=csharp,file=../csharp/Properties.cs,key=complexProperty]*)

(***include-output:complex***)

(**
It's perfectly fine to apply more than one label to a property; FsCheck displays all the applicable labels. 
This is useful for displaying intermediate results, for example:*)

(***define-output:multiply***)
let multiply (n: int, m: int) =
    let res = n*m
    sprintf "evidence = %i" res @| (
      "div1" @| (m <> 0 ==> lazy (res / m = n)) .&. 
      "div2" @| (n <> 0 ==> lazy (res / n = m)) .&. 
      "lt1"  @| (res > m) .&. 
      "lt2"  @| (res > n))
Check.Quick multiply

(**
    [lang=csharp,file=../csharp/Properties.cs,key=multipleLabels]*)

(***include-output:multiply***)

(**
Notice that the above property combines subproperties by tupling them. This works for tuples up to length 6 and lists:

*    `(<property1>,<property2>,...,<property6>)` means `<property1> .&. <property2> .&.... .&.<property6>`
*    `[property1;property2,...,propertyN]` means `<property1> .&. <property2> .&.... .&.<propertyN>`

The example written as a list:*)

let multiplyAsList (n: int, m: int) =
    let res = n*m
    sprintf "evidence = %i" res @| [
      "div1" @| (m <> 0 ==> lazy (res / m = n));
      "div2" @| (n <> 0 ==> lazy (res / n = m));
      "lt1"  @| (res > m);
      "lt2"  @| (res > n)]
(**
Produces the same result.*)
