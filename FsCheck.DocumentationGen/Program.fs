
module Bla

open System.Text
open FsCheck
open System
open System.IO

let lastOutput = ref ""
let docRunner =
    { new IRunner with
          member x.OnStartFixture t = lastOutput := !lastOutput + Runner.onStartFixtureToString t
          member x.OnArguments(ntest,args,every) = lastOutput := !lastOutput + every ntest args
          member x.OnShrink(args,everyShrink) = lastOutput := !lastOutput + everyShrink args
          member x.OnFinished(name,testResult) = lastOutput := !lastOutput + Runner.onFinishedToString name testResult
    }
let getOutput prop =
    lastOutput := ""
    Check.One({Config.Default with Runner = docRunner},prop)
    !lastOutput
    
let getOutputAll (t:Type) =
    lastOutput := ""
    Check.All({Config.Default with Runner = docRunner},t)
    !lastOutput

module QuickStart =
    let RevRevIsOrig (xs:list<int>) = List.rev(List.rev xs) = xs
    Check.Quick RevRevIsOrig
    let RevIsOrig (xs:list<int>) = List.rev xs = xs
    Check.Quick RevIsOrig

    type ListProperties =
        static member RevRevIsOrig xs = RevRevIsOrig xs
        static member RevIsOrig xs = RevIsOrig xs
    Check.QuickAll<ListProperties>()
    
    Check.QuickAll typeof<ListProperties>.DeclaringType
    
    let RevRevIsOrigFloat (xs:list<float>) = List.rev(List.rev xs) = xs
    Check.Quick RevRevIsOrigFloat

module Properties =

    let RevRevIsOrig xs = List.rev(List.rev xs) = xs
    let RevRevIsOrigInt (xs:list<int>) = List.rev(List.rev xs) = xs
    
    let rec private ordered xs = 
        match xs with
        | [] -> true
        | [x] -> true
        | x::y::ys ->  (x <= y) && ordered (y::ys)
    let rec private insert x xs = 
        match xs with
        | [] -> [x]
        | c::cs -> if x <= c then x::xs else c::(insert x cs)
     
    open Prop 
                         
    let Insert (x:int) xs = ordered xs ==> ordered (insert x xs)
    
    let Eager a = a <> 0 ==> (1/a = 1/a)
    Check.Quick Eager
    
    let Lazy a = a <> 0 ==> (lazy (1/a = 1/a))
    Check.Quick Lazy
    
    let orderedList = Arb.from<list<int>> |> Arb.mapFilter List.sort ordered
    let InsertWithArb x = forAll orderedList (fun xs -> ordered(insert x xs))
    Check.Quick InsertWithArb
    
    let ExpectDivideByZero() = throws<DivideByZeroException,_> (lazy (raise <| DivideByZeroException()))
    Check.Quick ExpectDivideByZero
    
    let TimesOut (a:int) = 
        lazy
            if a>10 then
                while true do System.Threading.Thread.Sleep(1000)
                true
            else 
                true
        |> within 2000
        
    let insertTrivial (x:int) xs = 
        ordered xs ==> (ordered (insert x xs))
        |> trivial (List.length xs = 0)
    Check.Quick insertTrivial

    //Classifying test cases
    let insertClassify (x:int) xs = 
        ordered xs ==> (ordered (insert x xs))
        |> classify (ordered (x::xs)) "at-head"
        |> classify (ordered (xs @ [x])) "at-tail" 
    Check.Quick insertClassify
        
    //Collecting data values
    let insertCollect (x:int) xs = 
        ordered xs ==> (ordered (insert x xs))
            |> collect (List.length xs)
    Check.Quick insertCollect

    //Combining observations
    let insertCombined (x:int) xs = 
        ordered xs ==> (ordered (insert x xs))
            |> classify (ordered (x::xs)) "at-head"
            |> classify (ordered (xs @ [x])) "at-tail"
            |> collect (List.length xs)
    Check.Quick insertCombined
    
    let complex (m: int) (n: int) =
        let res = n + m
        (res >= m)    |@ "result > #1" .&.
        (res >= n)    |@ "result > #2" .&.
        (res < m + n) |@ "result not sum"
    Check.Quick complex

    let multiply (n: int, m: int) =
      let res = n*m
      sprintf "evidence = %i" res @| (
        "div1" @| (m <> 0 ==> lazy (res / m = n)),
        "div2" @| (n <> 0 ==> lazy (res / n = m)),
        "lt1"  @| (res > m),
        "lt2"  @| (res > n))
    Check.Quick multiply
    
    let multiplyAsList (n: int, m: int) =
      let res = n*m
      sprintf "evidence = %i" res @| [
        "div1" @| (m <> 0 ==> lazy (res / m = n));
        "div2" @| (n <> 0 ==> lazy (res / n = m));
        "lt1"  @| (res > m);
        "lt2"  @| (res > n)]
    Check.Quick multiplyAsList

///Parametrized on functions to print the various bits and pieces.
let fsCheckDocGen head title p code output =
    let buffer = ref (new StringBuilder())
    let head = head buffer
    let title = title buffer
    let p = p buffer
    let code = code buffer
    let fsi s = "> " + s + ";;\n"
    let output = output buffer
    let fsiOutput s prop = output ((fsi s) + getOutput prop)
    let fsiOutputAll s t = output ((fsi s) + getOutputAll t)
    let write (filename:string) =
        File.WriteAllText(filename, buffer.Value.ToString())
        buffer := new StringBuilder()
    
    head "QuickStart"
    
    title "A Simple Example"
    p "A simple example of a property definition is"
    code "let RevRevIsOrig (xs:list<int>) = List.rev(List.rev xs) = xs"
    p "This property asserts that the reverse of the reverse of a list is the list itself. To check the property, we load this definition in F# interactive and then invoke"
    fsiOutput "Check.Quick RevRevIsOrig" QuickStart.RevRevIsOrig
    p "When a property fails, FsCheck displays a counter-example. For example, if we define "
    code "let RevIsOrig (xs:list<int>) = List.rev xs = xs"
    p "then checking it results in"
    fsiOutput "Check.Quick RevIsOrig" QuickStart.RevIsOrig
    p "FsCheck also shrinks the counter example, so that it is the minimal counter example that still fails the test case. In the example above, we see that the counter example is indeed minimal: the list must have at least two different elements. FsCheck also displays how many times it found a smaller (in some way) counter example and so proceeded to shrink further."
    
    title "Using FsCheck"
    p "To use FsCheck, you download the latest FsCheck source or binary. Build and reference the assembly in any projects containing specifications or test data generators. You can then test properties by loading the module they are defined in into F# interactive, and calling "
    code "Check.Quick <propertyName>"
    p "or by running and writing a small console application that calls the Check function. Integration with unit test runners such as xUnit and NUnit is possible as well - see Usage Tips for an example."
    
    title "Grouping properties"
    p "Usually, you'll write more than one property to test. FsCheck allows you to group together properties as static members of a class:"
    code @"type ListProperties =
    static member RevRevIsOrig xs = RevRevIsOrig xs
    static member RevIsOrig xs = RevIsOrig xs"
    p "These can be checked at once using the Check.QuickAll function:"
    fsiOutputAll "Check.QuickAll<ListProperties>()" typeof<QuickStart.ListProperties>
    p "FsCheck now also prints the name of each test."
    p "Since all top level functions of a a module are also compiled as static member of a class with the name of the module, you can also use quickCheck to test all the top level functions in a certain module. However, the type of a module is not directly accessible via F#, so you can use the following trick:"
    fsiOutputAll "Check.QuickAll typeof<ListProperties>.DeclaringType" typeof<QuickStart.ListProperties>.DeclaringType
    
    title "What do I do if a test loops or encounters an error?"
    p "In this case we know that the property does not hold, but Check.Quick does not display the counter-example. There is another testing function provided for this situation. Repeat the test using "
    code "Check.Verbose <property_name>"
    p "which displays each test case before running the test: the last test case displayed is thus the one in which the loop or error arises. Check.VerboseAll can be used with types and modules to check groups of properties verbosely."
    
    title "Caveat"
    p "The property above (the reverse of the reverse of a list is the list itself) is not always correct. Consider a list of floats that contains infinity, or nan (not a number). Since infinity <> infinity, and nan <> nan, the reverse of the reverse of {{[nan,nan]}} is not actually equal to {{[nan,nan]}} if you use straightforward element by element comparison. FsCheck has a knack for finding this kind of specification problem. However, since this behavior is seldom what you want, FsCheck only generates values that are 'neatly' comparable when you leave the type polymorphic (currently, unit, bool, char and string values). To see this error in action, force FsCheck to generate lists of floats:"
    code "let RevRevIsOrigFloat (xs:list<float>) = List.rev(List.rev xs) = xs"
    fsiOutput "Check.Quick RevRevIsOrigFloat" QuickStart.RevRevIsOrigFloat
    
    write "QuickStart.txt"
    
    head "Properties"
    p "Properties are expressed as F# function definitions. Properties are universally quantified over their parameters, so"
    code "let RevRevIsOrig xs = List.rev(List.rev xs) = xs"
    p "means that the equality holds for all lists xs."
    p @"Properties must not necessarily have monomorphic types.
'Polymorphic' properties, such as the one above will be tested by FsCheck as if the generic arguments are of type object; this means, that values of various simple types (bool, char, string,...) are generated. It may even be the case that one generated list contains more than one type, e.g. {['r', ""1a"", true]} would be a list that can be used to check the property above."
    p "The generated values are based on the type however, so you may change this behavior simply by giving xs a different inferred or explicit type:" 
    code "let RevRevIsOrigInt (xs:list<int>) = List.rev(List.rev xs) = xs"
    p "is only checked with lists of int."
    p @"FsCheck can check properties of various forms - these forms are called testable, and are indicated in the API by a generic type called 'Testable. A 'Testable may be a function of any number of parameters that returns bool or unit. In the latter case, a test passes if it does not throw."
    
    title "Conditional Properties"
    p @"Properties may take the form
<condition> ==> <property>"
    p "For example, "
    code @"let rec private ordered xs = 
        match xs with
        | [] -> true
        | [x] -> true
        | x::y::ys ->  (x <= y) && ordered (y::ys)
    let rec private insert x xs = 
        match xs with
        | [] -> [x]
        | c::cs -> if x <= c then x::xs else c::(insert x cs)
     
    open Prop 
                         
    let Insert (x:int) xs = ordered xs ==> ordered (insert x xs)"
    p @"Such a property holds if the property after ==> holds whenever the condition does."
    p @"Testing discards test cases which do not satisfy the condition. Test case generation continues until 100 cases which do satisfy the condition have been found, or until an overall limit on the number of test cases is reached (to avoid looping if the condition never holds). In this case a message such as"
    code "Arguments exhausted after 97 tests."
    p "indicates that 97 test cases satisfying the condition were found, and that the property held in those 97 cases. "
    p "Notice that in this case the generated values had to be restricted to int. This is because the generated values need to be comparable, but this is not reflected in the types. Therefore, without the explicit restriction, FsCheck could generate lists containing different types (subtypes of objects), and these are not mutually comparable."
    
    title "Lazy Properties"
    p "Since F# has eager evaluation by default, the above property does more work than necessary: it evaluates the property at the right of the condition no matter what the outcome of the condition on the left. While only a performance consideration in the above example, this may limit the expressiveness of properties - consider:"
    code "let Eager a = a <> 0 ==> (1/a = 1/a)"
    fsiOutput "Check.Quick Eager" Properties.Eager
    p "Lazy evaluation is needed here to make sure the propery is checked correctly:"
    code "let Lazy a = a <> 0 ==> (lazy (1/a = 1/a))"
    fsiOutput "Check.Quick Lazy" Properties.Lazy
    
    title "Quantified Properties"
    p "Properties may take the form"
    code "forAll <arbitrary>  (fun <args> -> <property>)"
    p "For example,"
    code "let InsertWithArb x = forAll orderedList (fun xs -> ordered(insert x xs))"
    p "The first argument of forAll is an IArbitrary instance. Such an instance encapsulates a test data generator and a shrinker (more on the latter later). By supplying a custom generator, instead of using the default generator for that type, it is possible to control the distribution of test data. In the example, by supplying a custom generator for ordered lists, rather than filtering out test cases which are not ordered, we guarantee that 100 test cases can be generated without reaching the overall limit on test cases. Combinators for defining generators are described later."
    
    title "Expecting exceptions"
    p "You may want to test that a function or method throws an exception under certain circumstances. The following combinator helps:"
    code @"throws<'e :> exn,'a> Lazy<'a>"
    p "An example:"
    code @"let ExpectDivideByZero() = throws<DivideByZeroException,_> (lazy (raise <| DivideByZeroException()))"
    fsiOutput "Check.Quick ExpectDivideByZero" Properties.ExpectDivideByZero
    
    title "Timed Properties"
    p "Properties may take the form"
    code "within <timeout in ms> <Lazy<property>>"
    p "For example,"
    code @"let TimesOut (a:int) = 
    lazy
        if a>10 then
            while true do System.Threading.Thread.Sleep(1000)
            true
        else 
            true
    |> within 2000"
    fsiOutput "Check.Quick TimesOut" Properties.TimesOut
    p "The first argument is the maximal time the lazy property given may run. If it runs longer, FsCheck considers the test as failed. Otherwise, the outcome of the lazy property is the outcome of within. Note that, although within attempts to cancel the thread in which the property is executed, that may not succeed, and so the thread may actually continue to run until the process ends."
    
    title "Observing Test Case Distribution"
    p "It is important to be aware of the distribution of test cases: if test data is not well distributed then conclusions drawn from the test results may be invalid. In particular, the ==> operator can skew the distribution of test data badly, since only test data which satisfies the given condition is used."
    p "FsCheck provides several ways to observe the distribution of test data. Code for making observations is incorporated into the statement of properties, each time the property is actually tested the observation is made, and the collected observations are then summarized when testing is complete."

    title "Counting Trivial Cases"
    p "A property may take the form"
    code "trivial <condition> <property>"
    p "For example,"
    code @"let InsertTrivial (x:int) xs = 
        ordered xs ==> (ordered (insert x xs))
        |> trivial (List.length xs = 0)"
    p "Test cases for which the condition is true are classified as trivial, and the proportion of trivial test cases in the total is reported. In this example, testing produces"
    fsiOutput "Check.Quick insertTrivial" Properties.insertTrivial

    title "Classifying Test Cases"
    p "A property may take the form"
    p "classify <condition> <string> <property>"
    p "For example,"
    code @"let InsertClassify (x:int) xs = 
    ordered xs ==> (ordered (insert x xs))
    |> classify (ordered (x::xs)) ""at-head""
    |> classify (ordered (xs @ [x])) ""at-tail"" "
    p "Test cases satisfying the condition are assigned the classification given, and the distribution of classifications is reported after testing. In this case the result is"
    fsiOutput "Check.Quick insertClassify" Properties.insertClassify
    p "Note that a test case may fall into more than one classification."

    title "Collecting Data Values"
    p "A property may take the form"
    code "collect <expression> <property>"
    p "For example,"
    code @"let InsertCollect (x:int) xs = 
    ordered xs ==> (ordered (insert x xs))
        |> collect (List.length xs)"
    p @"The argument of collect is evaluated in each test case, and the distribution of values is reported. The type of this argument is printed using sprintf ""%A"". In the example above, the output is"
    fsiOutput "Check.Quick insertCollect" Properties.insertCollect
    
    title "Combining Observations"
    p "The observations described here may be combined in any way. All the observations of each test case are combined, and the distribution of these combinations is reported. For example, testing the property"
    code @"let InsertCombined (x:int) xs = 
    ordered xs ==> (ordered (insert x xs))
        |> classify (ordered (x::xs)) ""at-head""
        |> classify (ordered (xs @ [x])) ""at-tail""
        |> collect (List.length xs)"
    p "produces"
    fsiOutput "Check.Quick insertCombined" Properties.insertCombined
    
    title "And, Or and Labelling Subproperties"
    p "Properties may take the form"
    code "<property> .&. <property>"
    code "<property> .|. <property>"
    p "p1.&. p2 succeeds if both succeed, fails if one of the properties fails, and is rejected when both are rejected."
    p "p1 .|. p2 succeeds if either property succeeds, fails if both properties fail, and is rejected when both are rejected."
    p "The .&. combinator is most commonly used to write complex properties which share a generator. In that case, it might be difficult upon failure to know excatly which sub-property has caused the failure. That's why you can label sub-properties, and FsCheck shows the labels of the failed subproperties when it finds a counter-example. This takes the form:"
    code "<string> @| <property>"
    code "<property> |@ <string>"
    p "For example,"
    code @"let complex (m: int) (n: int) =
    let res = n + m
    (res >= m)    |@ ""result > #1"" .&.
    (res >= n)    |@ ""result > #2"" .&.
    (res < m + n) |@ ""result not sum"""
    p "produces:"
    fsiOutput "Check.Quick complex" Properties.complex
    p "It's perfectly fine to apply more than one label to a property; FsCheck displays all the applicable labels. This is useful for displaying intermediate results, for example:"   
    code @"let multiply (n: int, m: int) =
  let res = n*m
  sprintf ""evidence = %i"" res @| (
    ""div1"" @| (m <> 0 ==> lazy (res / m = n)),
    ""div2"" @| (n <> 0 ==> lazy (res / n = m)),
    ""lt1""  @| (res > m),
    ""lt2""  @| (res > n))"
    fsiOutput "Check.Quick multiply" Properties.multiply
    p "Notice that the above property combines subproperties by tupling them. This works for tuples up to length 6. It also works for lists. In general form"
    code "(<property1>,<property2>,...,<property6>) means <property1> .&. <property2> .&.... .&.<property6>"
    code "[property1;property2,...,propertyN] means <property1> .&. <property2> .&.... .&.<propertyN>"
    p "The example written as a list:"
    code @" let multiplyAsList (n: int, m: int) =
  let res = n*m
  sprintf ""evidence = %i"" res @| [
    ""div1"" @| (m <> 0 ==> lazy (res / m = n));
    ""div2"" @| (n <> 0 ==> lazy (res / n = m));
    ""lt1""  @| (res > m);
    ""lt2""  @| (res > n)]"
    p "Produces the same result."
    
    write "Properties.txt"
    
    
    
[<EntryPoint>]
let main args =
    fsCheckDocGen 
        (fun buf txt -> buf.Value.AppendFormat("!!{0}\n\n",txt) |> ignore)
        (fun buf txt -> buf.Value.AppendFormat("*{0}*\n\n",txt) |> ignore)
        (fun buf txt -> buf.Value.AppendFormat("{0}\n",txt) |> ignore)
        (fun buf txt -> buf.Value.AppendFormat("\n{{{{\n{0}\n}}}}\n",txt) |> ignore)
        (fun buf txt -> buf.Value.AppendFormat("\n{{{{\n{0}\n}}}}\n",txt) |> ignore)
    System.Console.ReadKey() |> ignore
    0
