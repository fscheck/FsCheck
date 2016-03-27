(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2015 Kurt Schelfthout and contributors.              **
**  All rights reserved.                                                    **
**  https://github.com/fscheck/FsCheck                              **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

namespace FsCheck.Experimental

open System.Runtime.CompilerServices
open System.Reflection
open System.Threading
open System
open FsCheck

[<AbstractClass>]
type Setup<'Actual,'Model>() =
    ///Randomly generate the initial state of the actual object. Should still correspond to the 
    ///initial state of model object; so you should only randomly generate parameters to the instance
    ///that don't affect the model.
    ///Note:make sure that each value is truly a new instance if the commands change the state
    ///of the object. Gen.connstant in particular is a bad idea - use Gen.fresh instead.
    abstract Actual : unit -> 'Actual
    ///Initial state of model object. Must correspond to initial state of actual object.
    abstract Model : unit -> 'Model
    override __.ToString() = sprintf "Setup %s" typeof<'Actual>.Name

///An operation describes pre and post conditions and the model for a single operation under test.
///The post-conditions are the invariants that will be checked; when these do not hold the test fails.
[<AbstractClass>]
type Operation<'Actual,'Model>() =
    ///Excecutes the command on the object under test, and returns a property that must hold.
    ///This property typically compares the state of the model with the state of the object after
    ///execution of the command.
    abstract Check : 'Actual * 'Model  -> Property
    ///Executes the command on the model of the object.
    abstract Run : 'Model -> 'Model
    ///Optional precondition for execution of the command. When this does not hold, the test continues
    ///but the command is not executed.
    abstract Pre : 'Model -> bool
    ///The default precondition is true.
    default __.Pre _ = true

type TearDown<'Actual>() =
    abstract Actual : 'Actual -> unit
    default __.Actual _ = ()
    override __.ToString() = sprintf "TearDown %s" typeof<'Actual>.Name

///Defines the initial state for actual and model object, and allows to define the generator to use
///for the next state, based on the model.
[<AbstractClass>]
type Machine<'Actual,'Model>() =
    abstract Setup : Arbitrary<Setup<'Actual,'Model>>
    abstract TearDown : TearDown<'Actual>
    default __.TearDown = TearDown<_>()
    ///Generate a number of possible commands based on the current state of the model. 
    ///Preconditions are still checked, so even if a Command is returned, it is not chosen
    ///if its precondition does not hold.
    abstract Next : 'Model -> Gen<Operation<'Actual,'Model>>

type MachineRun<'Actual, 'Model> =
    { Setup : 'Model * Setup<'Actual,'Model> 
      Operations : list<Operation<'Actual,'Model> * 'Model>
      TearDown : TearDown<'Actual> }

//a single assignment variable
type OperationResult(?result,?name) =
    static let mutable resultCounter = 0
    static let nextCounter() = Interlocked.Increment &resultCounter
    let name = defaultArg name "result"
    let mutable counter = nextCounter()
    let mutable result = result
    member __.Get = result
    member __.Set v = match result with None -> result <- Some v | Some _ -> invalidOp "Result already set to value."
    override __.ToString() = sprintf "%s_%i" name counter

type ObjectMachineModel = OperationResult * OperationResult * string

type New<'Actual>(ctor:ConstructorInfo, parameters:array<obj>) =
    inherit Setup<'Actual,ObjectMachineModel>()
    let paramstring = 
        parameters
        |> Seq.map (sprintf "%O") 
        |> String.concat ", "
    override __.Actual() = ctor.Invoke parameters :?> 'Actual
    override __.Model() = 
        let result = OperationResult()
        let str = sprintf "let %O = new %s(%s)" result typeof<'Actual>.Name paramstring
        result, result, str

type MethodCall<'Actual>(meth:MethodInfo, parameters:array<obj>) =
    inherit Operation<'Actual,ObjectMachineModel>()
    let paramstring = 
                  parameters
                  |> Seq.map (sprintf "%O") 
                  |> String.concat ", " 
    override __.Run ((objectUnderTest, _, _)) =
        let result = OperationResult()
        let str = sprintf "let %O = %O.%s(%s)" result objectUnderTest meth.Name paramstring
        objectUnderTest, result, str
    override __.Check(actual, (_, modelResult, _)) =
        let result = lazy let result = meth.Invoke(actual, parameters)
                          modelResult.Set result
        Prop.ofTestable result
    override __.ToString() =
        sprintf "%s(%s)" meth.Name paramstring

type DisposeCall<'Actual>() =
    inherit TearDown<'Actual>()
    override __.Actual actual = match box actual with :? IDisposable as d -> d.Dispose() | _ -> ()
    override __.ToString() = if typeof<IDisposable>.GetTypeInfo().IsAssignableFrom (typeof<'Actual>.GetTypeInfo()) then sprintf "Dispose" else "Nothing"

type ObjectMachine<'Actual>(?methodFilter:MethodInfo -> bool) = 
    inherit Machine<'Actual,ObjectMachineModel>()
    static let skipMethods = [ "GetType"; "Finalize"; "MemberwiseClone"; "Dispose"; "System-IDisposable-Dispose"] |> Set.ofList
    let methodFilter = defaultArg methodFilter (fun mi -> not <| Set.contains mi.Name skipMethods)
    let parameterGenerator (parameters:seq<ParameterInfo>) =
        parameters |> Seq.map (fun p -> Arb.getGenerator p.ParameterType) |> Gen.sequence

    let ctors = 
        typeof<'Actual>.GetTypeInfo().DeclaredConstructors
        |> Seq.map (fun ctor -> 
                        gen { let! parameters = parameterGenerator (ctor.GetParameters())
                              return New<'Actual>(ctor, List.toArray parameters) :> Setup<'Actual,ObjectMachineModel> })
        |> Gen.oneof

    let instanceMethods =
        typeof<'Actual>.GetRuntimeMethods()
        |> Seq.filter methodFilter
        |> Seq.map (fun meth -> 
                        gen { let! parameters = parameterGenerator (meth.GetParameters())
                              return MethodCall<'Actual>(meth, List.toArray parameters) :> Operation<'Actual,ObjectMachineModel> })
        |> Gen.oneof

    override __.Setup = ctors |> Arb.fromGen
    override __.TearDown = upcast DisposeCall<'Actual>()
    override __.Next _ = instanceMethods

module StateMachine =
    open System
    open System.ComponentModel
    open Prop

    [<CompiledName("Setup"); EditorBrowsable(EditorBrowsableState.Never)>]
    let setup actual model =
        { new Setup<_,_>() with
            override __.Actual() = actual()
            override __.Model() = model() }

    [<CompiledName("Setup"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let setupFunc (actual:Func<_>) (model:Func<_>) =
        { new Setup<_,_>() with
            override __.Actual() = actual.Invoke()
            override __.Model() = model.Invoke() }

    [<CompiledName("TearDown"); EditorBrowsable(EditorBrowsableState.Never)>]
    let tearDown run =
        { new TearDown<_>() with
            override __.Actual actual = run actual }

    [<CompiledName("TearDown"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let tearDownAction (run:Action<_>) =
        { new TearDown<_>() with
            override __.Actual actual = run.Invoke actual }

    [<CompiledName("Operation"); EditorBrowsable(EditorBrowsableState.Never)>]
    let operationWithPrecondition name preCondition runModel check =
        { new Operation<'Actual,'Model>() with
            override __.Run pre = runModel pre
            override __.Check(model,actual) = check (model,actual) |> Prop.ofTestable
            override __.Pre model = preCondition model
            override __.ToString() = name }

    [<CompiledName("Operation"); EditorBrowsable(EditorBrowsableState.Never)>]
    let operation name runModel check =
        { new Operation<'Actual,'Model>() with
            override __.Run pre = runModel pre
            override __.Check(model,actual) = check (model,actual) |> Prop.ofTestable
            override __.ToString() = name }

    [<CompiledName("Operation"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let operationProp<'Actual,'Model> name (runModel:Func<'Model,_>) (check:Func<'Actual,_,Property>) =
        operation name runModel.Invoke (fun (a,b) -> check.Invoke(a,b))

    [<CompiledName("Operation"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let operationBool<'Actual,'Model> name (runModel:Func<'Model,_>) (check:Func<'Actual,_,bool>) =
        operation name runModel.Invoke (fun (a,b) -> Prop.ofTestable <| check.Invoke(a,b))

    [<CompiledName("Operation"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let operationAction<'Actual,'Model> name (runModel:Func<'Model,_>) (check:Action<'Actual,_>) =
        operation name runModel.Invoke (fun (a,b) -> Prop.ofTestable <| check.Invoke(a,b))

    [<CompiledName("Generate")>]
    let generate (spec:Machine<'Actual,'Model>) = 
        let rec genCommandsS state size =
            gen {
                if size > 0 then
                    let nextOperation = spec.Next state
                    let! command = nextOperation |> Gen.suchThatOption (fun operation -> operation.Pre state)
                    if Option.isNone command then return [state],[]
                    else
                        let! states, commands = genCommandsS (command.Value.Run state) (size-1)
                        return state :: states, command.Value :: commands
                else
                    return [state],[]
            }
        gen { let! setup = spec.Setup |> Arb.toGen
              let initialModel = setup.Model()
              let! models,operations = genCommandsS initialModel |> Gen.sized
              return { Setup = initialModel, setup
                       Operations = List.zip operations (List.tail models) //first state is actually the initial state; so drop it.
                       TearDown = spec.TearDown }
        }

    [<CompiledName("Shrink")>]
    let shrink (spec:Machine<'Actual,'Model>) (run:MachineRun<_,_>) =
        let runModels initial (operations:list<Operation<'Actual,'Model>>) =
            operations 
            |> Seq.scan (fun (_,model) operation -> (operation.Pre model, operation.Run model)) (true,initial) 
            |> Seq.skip 1
            |> Seq.zip operations
            |> Seq.map (fun (op,(pre,model)) -> (pre, (op,model)))
            
        let operationShrinker l =
            let allSubsequences (l:list<_>) =
                seq { for i in 1..l.Length do
                        yield! Seq.windowed i l |> Seq.map Seq.toList
                }
            match l with
            | [] -> Seq.empty
            | x::xs -> 
                seq { yield! allSubsequences xs
                      yield! Seq.map (fun l -> x::l) (allSubsequences xs) |> Seq.where (fun l' -> List.length l' <> List.length l)
                }

        run.Operations 
        |> List.map fst
        |> operationShrinker
        //try to shrink the list of operations
        |> Seq.choose (fun operations -> 
                            let initialModel = fst run.Setup
                            let transitions = runModels initialModel operations
                            let ok = Seq.forall fst transitions
                            let newOperations = transitions |> Seq.map snd |> Seq.toList
                            if ok then Some { run with Operations = newOperations } else None)
        //try to srhink the initial setup state
        |> Seq.append (Arb.toShrink spec.Setup (snd run.Setup) |> Seq.map (fun create -> { run with Setup = create.Model(), create }))
        
    [<CompiledName("Check")>]
    let check { Setup = _,setup; Operations = operations; TearDown = teardown } =
        let rec run actual (operations:list<Operation<'Actual,'Model> * _>) property =
            match operations with
            | [] -> teardown.Actual actual; property
            | ((op,model)::ops) -> 
                let prop = op.Check(actual, model) |> Prop.ofTestable
                run actual ops (property .&. prop) //not great: should stop generating once error is found
        run (setup.Actual()) operations (Prop.ofTestable true)

    ///Turn a specification into a property.
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let toProperty (spec:Machine<'Actual,'Model>) = 
        forAll (Arb.fromGenShrink(generate spec, shrink spec)) check
//                |> Prop.trivial (l.Length=0)
//                |> Prop.classify (l.Length > 1 && l.Length <=6) "short sequences (between 1-6 commands)" 
//                |> Prop.classify (l.Length > 6) "long sequences (>6 commands)" ))

[<AbstractClass;Sealed;Extension>]
type StateMachineExtensions =
    [<Extension>]
    static member ToProperty(spec: Machine<'Actual,'Model>) = StateMachine.toProperty spec
