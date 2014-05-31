(*** hide ***)
#I @"../../src/FsCheck/bin/Release"
#r @"FsCheck"
open FsCheck
open System

(**
# Model-based Testing

FsCheck also allows you to test objects, which usually encapsulate internal 
state through a set of methods. FsCheck, through a very small extension, 
allows you to do model-based specification of a class under test. Consider the following class, 
with an artificial bug in it:*)

type Counter() =
  let mutable n = 0
  member x.Inc() = n <- n + 1
  member x.Dec() = if n > 2 then n <- n - 2 else n <- n - 1
  member x.Get = n
  member x.Reset() = n <- 0
  override x.ToString() = n.ToString()

(**
As a model to test this class we'll use an int value which is an abstraction of the object's internal state. 
With this idea in mind, you can write a specification as follows:*)

open FsCheck.Commands

let spec =
  let inc = 
      { new ICommand<Counter,int>() with
          member x.RunActual c = c.Inc(); c
          member x.RunModel m = m + 1
          member x.Post (c,m) = m = c.Get |> Prop.ofTestable
          override x.ToString() = "inc"}
  let dec = 
      { new ICommand<Counter,int>() with
          member x.RunActual c = c.Dec(); c
          member x.RunModel m = m - 1
          member x.Post (c,m) = m = c.Get |> Prop.ofTestable
          override x.ToString() = "dec"}
  { new ISpecification<Counter,int> with
      member x.Initial() = (new Counter(),0)
      member x.GenCommand _ = Gen.elements [inc;dec] }

(**
A specification is an object that implementents `ISpecification<'typeUnderTest,'modelType>`. It should return 
an initial object and an initial model of that object; and it should return a generator of ICommand objects.

Each ICommand object typically represents one method to call on the object under test, and describes what 
happens to the model and the object when the command is executed. Also, it asserts preconditions that 
need to hold before executing the command: FsCheck will not execute that command if the precondition doesn 
not hold. It asserts postconditions that should hold after a command is executed: FsCheck 
fails the test if a postcondition does not hold.

Preferably also override ToString in each command so that counterexamples can be printed.

A specification can be checked as follows:*)

(***define-output:spec***)
Check.Quick (asProperty spec)

(***include-output:spec***)

(**
Notice that not only has FsCheck found our 'bug', it has also produced the minimal sequence that leads to it.*)