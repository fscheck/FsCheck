## QuickStart
#### A Simple Example
A simple example of a property definition is

    let revRevIsOrig (xs:list<int>) = List.rev(List.rev xs) = xs
This property asserts that the reverse of the reverse of a list is the list itself. To check the property, we load this definition in F# interactive and then invoke

    > Check.Quick revRevIsOrig;;
    Ok, passed 100 tests.
    
When a property fails, FsCheck displays a counter-example. For example, if we define 

    let revIsOrig (xs:list<int>) = List.rev xs = xs
then checking it results in

    > Check.Quick revIsOrig;;
    Falsifiable, after 2 tests (2 shrinks) (StdGen (884019159,295727999)):
    [1; 0]
    
FsCheck also shrinks the counter example, so that it is the minimal counter example that still fails the test case. In the example above, we see that the counter example is indeed minimal: the list must have at least two different elements. FsCheck also displays how many times it found a smaller (in some way) counter example and so proceeded to shrink further.
#### Using FsCheck
To use FsCheck, you download the latest FsCheck source or binary. Build and reference the assembly in any projects containing specifications or test data generators. You can then test properties by loading the module they are defined in into F# interactive, and calling 

    Check.Quick <propertyName>
or by running and writing a small console application that calls the Check function. Integration with unit test runners such as xUnit and NUnit is possible as well - see Usage Tips for an example.
#### Grouping properties
Usually, you'll write more than one property to test. FsCheck allows you to group together properties as static members of a class:

    type ListProperties =
    static member ``reverse of reverse is original`` xs = RevRevIsOrig xs
    static member ``reverse is original`` xs = RevIsOrig xs
These can be checked at once using the Check.QuickAll function:

    > Check.QuickAll<ListProperties>();;
    --- Checking ListProperties ---
    ListProperties.reverse of reverse is original-Ok, passed 100 tests.
    ListProperties.reverse is original-Falsifiable, after 3 tests (3 shrinks) (StdGen (885249229,295727999)):
    [1; 0]
    
FsCheck now also prints the name of each test.
Since all top level functions of a a module are also compiled as static member of a class with the name of the module, you can also use Check.QuickAll to test all the top level functions in a certain module. However, the type of a module is not directly accessible via F#, so you can use the following trick:

    > Check.QuickAll typeof<ListProperties>.DeclaringType;;
    --- Checking QuickStart ---
    QuickStart.revRevIsOrig-Ok, passed 100 tests.
    QuickStart.revIsOrig-Falsifiable, after 6 tests (7 shrinks) (StdGen (885549247,295727999)):
    [1; 0]
    QuickStart.revRevIsOrigFloat-Falsifiable, after 10 tests (4 shrinks) (StdGen (885679254,295727999)):
    [nan]
    
#### What do I do if a test loops or encounters an error?
In this case we know that the property does not hold, but Check.Quick does not display the counter-example. There is another testing function provided for this situation. Repeat the test using 

    Check.Verbose <property_name>
which displays each test case before running the test: the last test case displayed is thus the one in which the loop or error arises. Check.VerboseAll can be used with types and modules to check groups of properties verbosely.
#### Caveat
The property above (the reverse of the reverse of a list is the list itself) is not always correct. Consider a list of floats that contains infinity, or nan (not a number). Since infinity <> infinity, and nan <> nan, the reverse of the reverse of {{[nan,nan]}} is not actually equal to {{[nan,nan]}} if you use straightforward element by element comparison. FsCheck has a knack for finding this kind of specification problem. However, since this behavior is seldom what you want, FsCheck only generates values that are 'neatly' comparable when you leave the type polymorphic (currently, unit, bool, char and string values). To see this error in action, force FsCheck to generate lists of floats:

    let revRevIsOrigFloat (xs:list<float>) = List.rev(List.rev xs) = xs

    > Check.Quick revRevIsOrigFloat;;
    Falsifiable, after 19 tests (12 shrinks) (StdGen (886719313,295727999)):
    [nan]
    
## Properties
Properties are expressed as F# function definitions. Properties are universally quantified over their parameters, so

    let revRevIsOrig xs = List.rev(List.rev xs) = xs
means that the equality holds for all lists xs.
Properties must not necessarily have monomorphic types.
'Polymorphic' properties, such as the one above will be tested by FsCheck as if the generic arguments are of type object; this means, that values of various simple types (bool, char, string,...) are generated. It may even be the case that one generated list contains more than one type, e.g. {['r', "1a", true]} would be a list that can be used to check the property above.
The generated values are based on the type however, so you may change this behavior simply by giving xs a different inferred or explicit type:

    let revRevIsOrigInt (xs:list<int>) = List.rev(List.rev xs) = xs
is only checked with lists of int.
FsCheck can check properties of various forms - these forms are called testable, and are indicated in the API by a generic type called 'Testable. A 'Testable may be a function of any number of parameters that returns bool or unit. In the latter case, a test passes if it does not throw.
#### Conditional Properties
Properties may take the form
<condition> ==> <property>
For example, 

    let rec private ordered xs = 
    match xs with
    | [] -> true
    | [x] -> true
    | x::y::ys ->  (x <= y) && ordered (y::ys)

    let rec private insert x xs = 
    match xs with
    | [] -> [x]
    | c::cs -> if x <= c then x::xs else c::(insert x cs)

    let Insert (x:int) xs = ordered xs ==> ordered (insert x xs)
Such a property holds if the property after ==> holds whenever the condition does.
Testing discards test cases which do not satisfy the condition. Test case generation continues until 100 cases which do satisfy the condition have been found, or until an overall limit on the number of test cases is reached (to avoid looping if the condition never holds). In this case a message such as

    Arguments exhausted after 97 tests.
indicates that 97 test cases satisfying the condition were found, and that the property held in those 97 cases. 
Notice that in this case the generated values had to be restricted to int. This is because the generated values need to be comparable, but this is not reflected in the types. Therefore, without the explicit restriction, FsCheck could generate lists containing different types (subtypes of objects), and these are not mutually comparable.
#### Lazy Properties
Since F# has eager evaluation by default, the above property does more work than necessary: it evaluates the property at the right of the condition no matter what the outcome of the condition on the left. While only a performance consideration in the above example, this may limit the expressiveness of properties - consider:

    let Eager a = a <> 0 ==> (1/a = 1/a)

    > Check.Quick Eager;;
    Falsifiable, after 1 test (0 shrinks) (StdGen (886889323,295727999)):
    0
    with exception:
    System.DivideByZeroException: Attempted to divide by zero.
       at Properties.Eager(Int32 a) in C:\Users\Kurt\Projects\FsCheck\FsCheck\FsCheck.Documentation\Properties.fs:line 24
       at DocumentationGen.fsCheckDocGen@130-3.Invoke(Int32 a) in C:\Users\Kurt\Projects\FsCheck\FsCheck\FsCheck.Documentation\Program.fs:line 130
       at FsCheck.Testable.evaluate[a,b](FSharpFunc`2 body, a a) in C:\Users\Kurt\Projects\FsCheck\FsCheck\FsCheck\Property.fs:line 168
    
Lazy evaluation is needed here to make sure the propery is checked correctly:

    let Lazy a = a <> 0 ==> (lazy (1/a = 1/a))

    > Check.Quick Lazy;;
    Ok, passed 100 tests.
    
#### Quantified Properties
Properties may take the form

    forAll <arbitrary>  (fun <args> -> <property>)
For example,

    let InsertWithArb x = forAll orderedList (fun xs -> ordered(insert x xs))
The first argument of forAll is an IArbitrary instance. Such an instance encapsulates a test data generator and a shrinker (more on the latter later). By supplying a custom generator, instead of using the default generator for that type, it is possible to control the distribution of test data. In the example, by supplying a custom generator for ordered lists, rather than filtering out test cases which are not ordered, we guarantee that 100 test cases can be generated without reaching the overall limit on test cases. Combinators for defining generators are described later.
#### Expecting exceptions
You may want to test that a function or method throws an exception under certain circumstances. The following combinator helps:

    throws<'e :> exn,'a> Lazy<'a>
An example:

    let ExpectDivideByZero() = throws<DivideByZeroException,_> (lazy (raise <| DivideByZeroException()))

    > Check.Quick ExpectDivideByZero;;
    Ok, passed 100 tests.
    
#### Timed Properties
Properties may take the form

    within <timeout in ms> <Lazy<property>>
For example,

    let timesOut (a:int) = 
    lazy
        if a>10 then
            while true do System.Threading.Thread.Sleep(1000)
            true
        else 
            true
    |> within 2000

    > Check.Quick timesOut;;
    Timeout of 2000 milliseconds exceeded, after 37 tests (0 shrinks) (StdGen (945192658,295727999)):
    11
    
The first argument is the maximal time the lazy property given may run. If it runs longer, FsCheck considers the test as failed. Otherwise, the outcome of the lazy property is the outcome of within. Note that, although within attempts to cancel the thread in which the property is executed, that may not succeed, and so the thread may actually continue to run until the process ends.
#### Observing Test Case Distribution
It is important to be aware of the distribution of test cases: if test data is not well distributed then conclusions drawn from the test results may be invalid. In particular, the ==> operator can skew the distribution of test data badly, since only test data which satisfies the given condition is used.
FsCheck provides several ways to observe the distribution of test data. Code for making observations is incorporated into the statement of properties, each time the property is actually tested the observation is made, and the collected observations are then summarized when testing is complete.
#### Counting Trivial Cases
A property may take the form

    trivial <condition> <property>
For example,

    let insertTrivial (x:int) xs = 
    ordered xs ==> (ordered (insert x xs))
    |> trivial (List.length xs = 0)
Test cases for which the condition is true are classified as trivial, and the proportion of trivial test cases in the total is reported. In this example, testing produces

    > Check.Quick insertTrivial;;
    Arguments exhausted after 55 tests (36% trivial).
    
#### Classifying Test Cases
A property may take the form
classify <condition> <string> <property>
For example,

    let insertClassify (x:int) xs = 
    ordered xs ==> (ordered (insert x xs))
    |> classify (ordered (x::xs)) "at-head"
    |> classify (ordered (xs @ [x])) "at-tail" 
Test cases satisfying the condition are assigned the classification given, and the distribution of classifications is reported after testing. In this case the result is

    > Check.Quick insertClassify;;
    Arguments exhausted after 54 tests.
    44% at-tail, at-head.
    24% at-head.
    22% at-tail.
    
Note that a test case may fall into more than one classification.
#### Collecting Data Values
A property may take the form

    collect <expression> <property>
For example,

    let insertCollect (x:int) xs = 
    ordered xs ==> (ordered (insert x xs))
        |> collect (List.length xs)
The argument of collect is evaluated in each test case, and the distribution of values is reported. The type of this argument is printed using sprintf "%A". In the example above, the output is

    > Check.Quick insertCollect;;
    Arguments exhausted after 70 tests.
    50% 0.
    32% 1.
    11% 2.
    4% 3.
    1% 4.
    
#### Combining Observations
The observations described here may be combined in any way. All the observations of each test case are combined, and the distribution of these combinations is reported. For example, testing the property

    let insertCombined (x:int) xs = 
    ordered xs ==> (ordered (insert x xs))
        |> classify (ordered (x::xs)) "at-head"
        |> classify (ordered (xs @ [x])) "at-tail"
        |> collect (List.length xs)
produces

    > Check.Quick insertCombined;;
    Arguments exhausted after 53 tests.
    24% 0, at-tail, at-head.
    18% 1, at-tail, at-head.
    18% 1, at-head.
    16% 1, at-tail.
    7% 2, at-head.
    5% 2, at-tail.
    3% 2.
    1% 5, at-tail.
    1% 4.
    
#### And, Or and Labelling Subproperties
Properties may take the form

    <property> .&. <property>

    <property> .|. <property>
p1.&. p2 succeeds if both succeed, fails if one of the properties fails, and is rejected when both are rejected.
p1 .|. p2 succeeds if either property succeeds, fails if both properties fail, and is rejected when both are rejected.
The .&. combinator is most commonly used to write complex properties which share a generator. In that case, it might be difficult upon failure to know excatly which sub-property has caused the failure. That's why you can label sub-properties, and FsCheck shows the labels of the failed subproperties when it finds a counter-example. This takes the form:

    <string> @| <property>

    <property> |@ <string>
For example,

    let complex (m: int) (n: int) =
    let res = n + m
    (res >= m)    |@ "result > #1" .&.
    (res >= n)    |@ "result > #2" .&.
    (res < m + n) |@ "result not sum"
produces:

    > Check.Quick complex;;
    Falsifiable, after 1 test (0 shrinks) (StdGen (995775551,295727999)):
    Label of failing property: result not sum
    0
    0
    
It's perfectly fine to apply more than one label to a property; FsCheck displays all the applicable labels. This is useful for displaying intermediate results, for example:

    let multiply (n: int, m: int) =
        let res = n*m
        sprintf "evidence = %i" res @| (
        "div1" @| (m <> 0 ==> lazy (res / m = n)),
        "div2" @| (n <> 0 ==> lazy (res / n = m)),
        "lt1"  @| (res > m),
        "lt2"  @| (res > n))

    > Check.Quick multiply;;
    Falsifiable, after 1 test (0 shrinks) (StdGen (996145572,295727999)):
    Labels of failing property: evidence = 0, lt1
    (0, 0)
    
Notice that the above property combines subproperties by tupling them. This works for tuples up to length 6. It also works for lists. In general form

    (<property1>,<property2>,...,<property6>) means <property1> .&. <property2> .&.... .&.<property6>

    [property1;property2,...,propertyN] means <property1> .&. <property2> .&.... .&.<propertyN>
The example written as a list:

     let multiplyAsList (n: int, m: int) =
        let res = n*m
        sprintf "evidence = %i" res @| [
        "div1" @| (m <> 0 ==> lazy (res / m = n));
        "div2" @| (n <> 0 ==> lazy (res / n = m));
        "lt1"  @| (res > m);
        "lt2"  @| (res > n)]
Produces the same result.
## Test data: generators, shrinkers and Arbitrary instances
Test data is produced by test data generators. FsCheck defines default generators for some often used types, but you can use your own, and will need to define your own generators for any new types you introduce.
Generators have types of the form Gen<'a>; this is a generator for values of type a. For manipulating values of type Gen, a computation expression called gen is provided by FsCheck, and all the functions in the Gen module are at your disposal.
Shrinkers have types of the for 'a -> seq<'a>; given a value, a shrinker produces a sequence of values that are in some way smaller than the given value. If FsCheck finds a set of values that falsify a given property, it will try to make that value smaller than the original (random) value by getting the shrinks for the value and trying each one in turn to check that the property is still false. If it is, the smaller value becomes the new counter example and the shrinking process continues with that value.
Shrinkers have no special support from FsCheck - this is not needed, since you have all that you need in seq computation expressions and the Seq module.
Finally, an Arbitrary<'a> instance packages these two types to be used in properties. FsCheck also allows you to register Arbitrary instances in a Type to Arbitrary dictionary. This dictionary is used to find an arbitrary instance for properties that have arguments, based on the argument's type.
Arbitrary instances have some helper functions in the Arb module.
#### Generators
Generators are built up from the function

    val choose : (int * int -> Gen<int>)
which makes a random choice of a value from an interval, with a uniform distribution. For example, to make a random choice between the elements of a list, use

    let chooseFromList xs = 
    gen { let! i = Gen.choose (0, List.length xs-1) 
          return (List.nth xs i) }
#### Choosing between alternatives
A generator may take the form
Gen.oneof <sequence of generators>
which chooses among the generators in the list with equal probability. For example,

    Gen.oneof [ gen { return true }; gen { return false } ]
generates a random boolean which is true with probability one half.
We can control the distribution of results using the function

    val frequency: seq<int * Gen<'a>> -> Gen<'a>
instead. Frequency chooses a generator from the list randomly, but weighs the probability of choosing each alternative by the factor given. For example,

    Gen.frequency [ (2, gen { return true }); (1, gen { return false })]
generates true two thirds of the time.
#### The size of test data
Test data generators have an implicit size parameter; FsCheck begins by generating small test cases, and gradually increases the size as testing progresses. Different test data generators interpret the size parameter in different ways: some ignore it, while the list generator, for example, interprets it as an upper bound on the length of generated lists. You are free to use it as you wish to control your own test data generators. 
You can obtain the value of the size parameter using

    val sized : ((int -> Gen<'a>) -> Gen<'a>)
sized g calls g, passing it the current size as a parameter. For example, to generate natural numbers in the range 0 to size, use

    Gen.sized <| fun s -> Gen.choose (0,s)
The purpose of size control is to ensure that test cases are large enough to reveal errors, while remaining small enough to test fast. Sometimes the default size control does not achieve this. For example, towards the end of a test run arbitrary lists may have up to 50 elements, so arbitrary lists of lists may have up to 2500, which is too large for efficient testing. In such cases it can be useful to modify the size parameter explicitly. You can do so using

    val resize : (int -> Gen<'a> -> Gen<'a>)
resize n g invokes generator g with size parameter n. The size parameter should never be negative. For example, to generate a random matrix it might be appropriate to take the square root of the original size:

    let matrix gen = Gen.sized <| fun s -> Gen.resize (s|>float|>sqrt|>int) gen
#### Generating recursive data types
Generators for recursive data types are easy to express using oneof or frequency to choose between constructors, and F#'s standard computation expression syntax to form a generator for each case. There are also map functions for arity up to 6 to lift constructors and functions into the Gen type. For example, if the type of trees is defined by

    type Tree = Leaf of int | Branch of Tree * Tree
then a generator for trees might be defined by

    let rec unsafeTree() = 
    Gen.oneof [ Gen.map Leaf Arb.generate<int> 
                Gen.map2 (fun x y -> Branch (x,y)) (unsafeTree()) (unsafeTree())]
However, a recursive generator like this may fail to terminate with a StackOverflowException, or produce very large results. To avoid this, recursive generators should always use the size control mechanism. For example,

    let tree =
    let rec tree' s = 
        match s with
        | 0 -> Gen.map Leaf Arb.generate<int>
        | n when n>0 -> 
            let subtree = tree' (n/2)
            Gen.oneof [ Gen.map Leaf Arb.generate<int> 
                        Gen.map2 (fun x y -> Branch (x,y)) subtree subtree]
        | _ -> invalidArg "s" "Only positive arguments are allowed"
    Gen.sized tree'
Note that 
* We guarantee termination by forcing the result to be a leaf when the size is zero. 
* We halve the size at each recursion, so that the size gives an upper bound on the number of nodes in the tree. We are free to interpret the size as we will. 
* The fact that we share the subtree generator between the two branches of a Branch does not mean that we generate the same tree in each case.

#### Useful Generator Combinators
If g is a generator for type t, then 
- `two g` generates a pair of t's, 
- `three g` generates a triple of t's, 
- `four g` generates a quadruple of t's, 
- If xs is a list, then `elements xs` generates an arbitrary element of xs.
- `listOfLength n g` generates a list of exactly n t's. 
- `listOf g` generates a list of t's whose length is determined by the size parameter
- `nonEmptyListOf g` generates a non-empty list of t's whose length is determined by the size parameter.
- `constant v` generates the value v.
- `suchThat p g` generates t's that satisfy the predicate p. Make sure there is a high chance that the predicate is satisfied.
- `suchThatOption p g` generates Some t's that satisfy the predicate p, and None if none are found. (After 'trying hard')

All the generator combinators are functions on the Gen module.
#### Default Generators and Shrinkers based on type
FsCheck defines default test data generators  and shrinkers for some often used types: unit, bool, byte, int, float, char, string, DateTime, lists, array 1D and 2D, Set, Map, objects and functions from and to any of the above. Furthermore, by using reflection, FsCheck can derive default implementations of record types, discriminated unions, tuples and enums in terms of any primitive types that are defined (either in FsCheck or by you).
You do not need to define these explicity for every property: FsCheck can provide a property with appropriate generators and shrinkers for all of the property's arguments, if it knows them or can derive them. Usually you can let type inference do the job of finding out these types based on your properties. However if you want to coerce FsCheck to use a particular generator and shrinker, you can do so by providing the appropriate type annotations.
As mentioned in the introduction, FsCheck packages a generator and shrinker for a particular type in an Arbitrary type. You can provide FsCheck with an Arbitrary instance for your own types, by defining static members of a class, each of which should return an instance of a subclass of the class Arbitrary<'a>:

    type MyGenerators =
    static member Tree() =
        {new Arbitrary<Tree>() with
            override x.Generator = tree
            override x.Shrinker t = Seq.empty }
Replace the 'a by the particular type you are defiing an Arbitary instance for. Only the Generator method needs to be defined; Shrinker by default returns the empty sequence (i.e. no shrinking will occur for this type).
Now, to register all Arbitrary instances in this class:

    Arb.register<MyGenerators>()
FsCheck now knows about Tree types, and can not only generate Tree values, but also e.g. lists, tuples and option values containing Trees:

    let RevRevTree (xs:list<Tree>) = 
    List.rev(List.rev xs) = xs

    > Check.Quick RevRevTree;;
    Ok, passed 100 tests.
    
To generate types with a generic type argument, e.g.:

    type Box<'a> = Whitebox of 'a | Blackbox of 'a
you can use the same principle. So the class MyGenerators can be writtten as follows:

    let boxGen<'a> : Gen<Box<'a>> = 
        gen { let! a = Arb.generate<'a>
              return! Gen.elements [ Whitebox a; Blackbox a] }

    type MyGenerators =
        static member Tree() =
            {new Arbitrary<Tree>() with
                override x.Generator = tree
                override x.Shrinker t = Seq.empty }
        static member Box() = Arb.fromGen boxGen
Notice that we use the function 'val generate<'a> : Gen<'a>' from the Arb module to get the generator for the type argument of Box. This allows you to define generators recursively. Similarly, there is a function shrink<'a>. Look at the FsCheck source for examples of default Arbitrary implementations to get a feeling of how to write such Arbitrary instances. The Arb module should help you with this task as well.
Now, the following property can be checked:

    let RevRevBox (xs:list<Box<int>>) = 
    List.rev(List.rev xs) = xs
    |> Prop.collect xs

    > Check.Quick RevRevBox;;
    Ok, passed 100 tests.
    11% [].
    2% [Blackbox 0].
    1% [Whitebox 9; Blackbox 3; Whitebox -2; Blackbox -8; Whitebox -13; Blackbox -19;
(etc)
Note that the class needs not be tagged with attributes in any way. FsCheck determines the type of the generator by the return type of each static member.
Also note that in this case we actually didn't need to write a generator or shrinker: FsCheck can derive suitable generators using reflection for discriminated unions, record types and enums.
#### Some useful methods on the Arb module
- `Arb.from<'a>` returns the registered Arbitrary instance for the given type 'a
- `Arb.fromGen` makes a new Arbitrary instance from just a given generator - the shrinker return the empty sequence
- `Arb.fromGenShrink` make a new Arbitrary instance from a given generator and shrinker. This is equivalent to implementing Arbitrary yourself, but may be shorter.
- `Arb.generate<'a>` returns the generator of the registered Arbitrary instance for the given type 'a
- `Arb.shrink` return the immediate shrinks of the registered Arbitrary instance for the given value
- `Arb.convert` given conversion functions to ('a ->'b) and from ('b ->'a), converts an Arbitrary<'a> instance to an Arbitrary<'b>
- `Arb.filter` filters the generator and shrinker for a given Arbitrary instance to contain only those values that match with the given filter function
- `Arb.mapFilter` maps the generator and filter the shrinkers for a given Arbitrary instance. Mapping the generator is sometimes faster, e.g. for a PositiveInt it is faster to take the absolute value than to filter the negative values.
- `Arb.Default` is a type that contains all the default Arbitrary instances as they are shipped and registerd by FsCheck by default. This is useful when you override a default generator - typically this is because you want to filter certain values from it, and then you need to be able to refer to the default generator in your overriding generator.

## Stateful Testing
FsCheck also allows you to test objects, which usually encapsulate internal state through a set of methods. FsCheck, through a very small extension, allows you to do model-based specification of a class under test. Consider the following class, with an artificial bug in it:

    type Counter() =
    let mutable n = 0
    member x.Inc() = n <- n + 1
    member x.Dec() = if n > 2 then n <- n - 2 else n <- n - 1
    member x.Get = n
    member x.Reset() = n <- 0
    override x.ToString() = n.ToString()
The obvious model to test this class is just an int value which will serve as an abstraction of the object's internal state. With this idea in mind, you can write a specification as follows:

    let spec =
        let inc = 
            { new ICommand<Counter,int>() with
                member x.RunActual c = c.Inc(); c
                member x.RunModel m = m + 1
                member x.Post (c,m) = m = c.Get 
                override x.ToString() = "inc"}
        let dec = 
            { new ICommand<Counter,int>() with
                member x.RunActual c = c.Dec(); c
                member x.RunModel m = m - 1
                member x.Post (c,m) = m = c.Get 
                override x.ToString() = "dec"}
        { new ISpecification<Counter,int> with
            member x.Initial() = (new Counter(),0)
            member x.GenCommand _ = Gen.elements [inc;dec] }
A specification is an object that Implementents ISpecification<'typeUnderTest,'modelType>. It should return an initial object and an initial model of that object; and it should return a generator of ICommand objects.
Each ICommand object typically represents one method to call on the object under test, and describes what happens to the model and the object when the command is executed. Also, it asserts preconditions that need to hold before executing the command: FsCheck will not execute that command if the precondittion doesn not hold. It asserts postconditions that should hold after a command is executed: FsCheck fails the test if a postcondition does not hold.
Preferably also override ToString in each command so that counterexamples can be printed.
A specification can be checked as follows:

    > Check.Quick (asProperty spec);;
    Falsifiable, after 6 tests (2 shrinks) (StdGen (1020916989,295727999)):
    [inc; inc; inc; dec]
    
Notice that not only has FsCheck found our 'bug', it has also produced the minimal sequence that leads to it.
## Usage tips
#### Properties of functions
Since FsCheck can generate random function values, it can check properties of functions. For example, we can check associativity of function composition as follows:

    let associativity (x:Tree) (f:Tree->float,g:float->char,h:char->int) = ((f >> g) >> h) x = (f >> (g >> h)) x

    > Check.Quick associativity;;
    Ok, passed 100 tests.
    
We can generate functions Tree -> _anything_. If a counter-example is found, function values will be displayed as "\<func\>".
However, FsCheck can show you the generated function in more detail, by using the Function type. If you use that, FsCheck can even shrink your function. For example:

    let mapRec (F (_,f)) (l:list<int>) =
    not l.IsEmpty ==>
        lazy (List.map f l = ((*f <|*) List.head l) :: List.map f (List.tail l))

    > Check.Quick mapRec;;
    Falsifiable, after 1 test (1 shrink) (StdGen (1028557426,295727999)):
    { 0->0; 1->0; 2->0 }
    [1]
    
The  type Function<'a,'b> =  F of ref<list<('a*'b)>> * ('a ->'b) records a map of all the arguments it was called with, and the result it produced. In your properties, you can extract the actual function by pattern matching as in the example. Function is used to print the function, and also to shrink it.
#### Use pattern matching instead of forAll to use custom generators

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
This make properties much more readable, especially since you can define custom shrink functions as well.
FsCheck has quite a few of these, e.g. NonNegativeInt, PositiveInt, StringWithoutNullChars etc. Have a look at the default Arbitrary instances on the Arb.Default type.
Also, if you want to define your own, the Arb.filter, Arb.convert and Arb.mapFilter functions will come in handy.
#### Running tests using only modules
Since Arbitrary instances are given as static members of classes, and properties can be grouped together as static members of classes, and since top level let functions are compiled as static member of their enclosing module (which is compiled as a class), you can simply define your properties and generators as top level let-bound functions, and then register all generators and and all properties at once using the following trick:

    let myprop =....
        let mygen =...
        let helper = "a string"
        let private helper' = true

    type Marker = class end
        Arb.register (typeof<Marker>.DeclaringType)
        Check.All (typeof<Marker>.DeclaringType)
The Marker type is just any type defined in the module, to be able to get to the module's Type. F# offers no way to get to a module's Type directly.
FsCheck determines the intent of the function based on its return type:
* Properties: return unit, bool, Property or function of any arguments to those types or Lazy value of any of those types
* Arbitrary instances: return Arbitrary<_>

All other functions are respectfully ignored. If you have top level functions that return types that FsCheck will do something with, but do not want them checked or registered, just make them private. FsCheck will ignore those functions.

#### Temporary fix to get the NUnit Addin working for your test:

In the project containing your tests (methods marked with [Property] from FsCheck.Nunit):
* Add a Reference to FsCheck.Nunit and FsCheck.NUnit.Addin
* Add a public class to your project that implements the addin

Here is one for F#:

    open NUnit.Core.Extensibility
    open FsCheck.NUnit
    open FsCheck.NUnit.Addin
    [<NUnitAddin(Description = "FsCheck addin")>]
    type FsCheckAddin() =        
        interface IAddin with
            override x.Install host = 
                let tcBuilder = new FsCheckTestCaseBuider()
                host.GetExtensionPoint("TestCaseBuilders").Install(tcBuilder)
                true
                
and this one will enable C# projects:

    using FsCheck.NUnit.Addin;
    using NUnit.Core.Extensibility;
    namespace FsCheck.NUnit.CSharpExamples
    {
        [NUnitAddin(Description = "FsCheck addin")]
        public class FsCheckNunitAddin : IAddin
        {
            public bool Install(IExtensionHost host)
            {
                var tcBuilder = new FsCheckTestCaseBuider();
                host.GetExtensionPoint("TestCaseBuilders").Install(tcBuilder);
                return true;
            }
        }
    }
    
After this you can declare flag your test with [Property] instead of [Test] like this:

    [<Property>]
    let maxLe (x:float) y = 
        (x <= y) ==> (lazy (max  x y = y))

The addin will run this using Check.One and Assert.Fail to flag failed tests.

#### Implementing IRunner to integrate FsCheck with mb|x|N|cs|Unit
The Config type that can be passed to the Check.One or Check.All methods takes an IRunner as argument. This interface has the following methods:
* OnStartFixture is called when FsCheck is testing all the methods on a type, before starting any tests.
* OnArguments is called after every test, passing the implementation the test number, the arguments and the every function. 
* OnShrink is called at every succesful shrink.
* OnFinished: is called with the name of the test and the outcome of the overall test run. This is used in the example below to call Assert statements from an outside unit testing framework - allowing FsCheck to integrate with a number of unit testing frameworks. You can leverage another unit testing framework's ability to setup and tear down tests, have a nice graphical runner etc.


        let xUnitRunner =
            { new IRunner with
                member x.OnStartFixture t = ()
                member x.OnArguments (ntest,args, every) = ()
                member x.OnShrink(args, everyShrink) = ()
                member x.OnFinished(name,testResult) = 
                    match testResult with 
                    | TestResult.True _ -> Assert.True(true)
                    | _ -> Assert.True(false, Runner.onFinishedToString name result) 
            }
   
        let withxUnitConfig = { Config.Default with Runner = xUnitRunner }
        
#### Implementing IRunner to customize printing of generated arguments
By default, FsCheck prints generated arguments using sprintf "%A", or structured formatting. This usually does what you expect, i.e. for primitive types the value, for objects the ToString override and so on. If it does not (A motivating case is testing with COM objects - overriding ToString is not an option and structured formatting does not do anything useful with it), you can use the label combinator to solve this on a per property basis, but a more structured solution can be achieved by implementing IRunner. For example:

        let formatterRunner =
            { new IRunner with
                member x.OnStartFixture t =
                    printf "%s" (Runner.onStartFixtureToString t)
                member x.OnArguments (ntest,args, every) =
                    printf "%s" (every ntest (args |> List.map formatter))
                member x.OnShrink(args, everyShrink) =
                    printf "%s" (everyShrink (args |> List.map formatter))
                member x.OnFinished(name,testResult) = 
                    let testResult' = match testResult with 
                                        | TestResult.False (testData,origArgs,shrunkArgs,outCome,seed) -> 
                                            TestResult.False (testData,origArgs |> List.map formatter, 
                                                              shrunkArgs |> List.map formatter,outCome,seed)
                                        | t -> t
                    printf "%s" (Runner.onFinishedToString name testResult') 
            }
#### An equality comparison that prints the left and right sides of the equality
Properties commonly check for equality. If a test case fails, FsCheck prints the counterexample, but sometimes it is useful to print the left and right side of the comparison as well, especially if you do some complicated calculations with the generated arguments first. To make this easier, you can define your own labelling equality combinator:

        let (.=.) left right = left = right |@ sprintf "%A = %A" left right
    
        let testCompare (i:int) (j:int) = 2*i+1  .=. 2*j-1

    > Check.Quick testCompare;;
    Falsifiable, after 1 test (0 shrinks) (StdGen (1029127459,295727999)):
    Label of failing property: 1 = -1
    0
    0
    
Of course, you can do this for any operator or function that you often use.
#### Some ways to use FsCheck
* By adding properties and generators to an fsx file in your project. It's easy to execute, just press ctrl-a and alt-enter, and the results are displayed in F# Interactive. Be careful when referencing dlls that are built in your solution; F# Interactive will lock those for the remainder of the session, and you won't be able to build unitl you quit the session. One solution is to include the source files instead of the dlls, but that makes the process slower. Useful for smaller projects. Difficult to debug, as far as I know.
* By making a separate console application. Easy to debug, no annoying locks on assemblies. Your best option if you use only FsCheck for testing and your properties span multiple assemblies.
* By using another unit testing framework. Useful if you have a mixed FsCheck/unit testing approach (some things are easier to check using unit tests, and vice versa), and you like a graphical runner. Depending on what unit testing framework you use, you may get good integration with Visual Studio for free. See above for ways to customize FsCheck for this scenario.
