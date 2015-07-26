(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2015 Kurt Schelfthout and contributors.              **  
**  All rights reserved.                                                    **
**  https://github.com/fscheck/FsCheck                              **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

namespace FsCheck

open System.Runtime.CompilerServices
open System.Reflection
open System.Threading
open System
open Common
open Microsoft.FSharp.Core.Operators.Unchecked

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
      Operations : list<'Model * Operation<'Actual,'Model>>
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
    override __.ToString() = if typeof<IDisposable>.IsAssignableFrom typeof<'Actual> then sprintf "Dispose" else "Nothing"

type ObjectMachine<'Actual>(?methodFilter:MethodInfo -> bool) = 
    inherit Machine<'Actual,ObjectMachineModel>()
    static let skipMethods = [ "GetType"; "Finalize"; "MemberwiseClone"; "Dispose"; "System-IDisposable-Dispose"] |> Set.ofList
    let methodFilter = defaultArg methodFilter (fun mi -> not <| Set.contains mi.Name skipMethods)
    let parameterGenerator (parameters:seq<ParameterInfo>) =
        parameters |> Seq.map (fun p -> p.ParameterType) |> Seq.map Arb.getGenerator |> Gen.sequence

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
    override __.Next model = instanceMethods

module Command =
    open System
    open System.ComponentModel
    open Prop

    [<CompiledName("Create"); EditorBrowsable(EditorBrowsableState.Never)>]
    let create actual model =
        { new Setup<_,_>() with
            override __.Actual() = actual()
            override __.Model() = model() }

    [<CompiledName("Create"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let createFunc (actual:Func<_>) (model:Func<_>) =
        { new Setup<_,_>() with
            override __.Actual() = actual.Invoke()
            override __.Model() = model.Invoke() }

    [<CompiledName("Destroy"); EditorBrowsable(EditorBrowsableState.Never)>]
    let destroy run =
        { new TearDown<_>() with
            override __.Actual actual = run actual }

    [<CompiledName("Destroy"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let destroyAction (run:Action<_>) =
        { new TearDown<_>() with
            override __.Actual actual = run.Invoke actual }

    [<CompiledName("FromFun"); EditorBrowsable(EditorBrowsableState.Never)>]
    let fromFunWithPrecondition name preCondition runModel check =
        { new Operation<'Actual,'Model>() with
            override __.Run pre = runModel pre
            override __.Check(model,actual) = check (model,actual) |> Prop.ofTestable
            override __.Pre model = preCondition model
            override __.ToString() = name }

    [<CompiledName("FromFun"); EditorBrowsable(EditorBrowsableState.Never)>]
    let fromFun name runModel check =
        { new Operation<'Actual,'Model>() with
            override __.Run pre = runModel pre
            override __.Check(model,actual) = check (model,actual) |> Prop.ofTestable
            override __.ToString() = name }

    [<CompiledName("FromFunc"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let fromFuncProp<'Actual,'Model> name (runModel:Func<'Model,_>) (check:Func<'Actual,_,Property>) =
        fromFun name runModel.Invoke (fun (a,b) -> check.Invoke(a,b))

    [<CompiledName("FromFunc"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let fromFuncBool<'Actual,'Model> name (runModel:Func<'Model,_>) (check:Func<'Actual,_,bool>) =
        fromFun name runModel.Invoke (fun (a,b) -> Prop.ofTestable <| check.Invoke(a,b))

    [<CompiledName("FromFunc"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let fromFuncAction<'Actual,'Model> name (runModel:Func<'Model,_>) (check:Action<'Actual,_>) =
        fromFun name runModel.Invoke (fun (a,b) -> Prop.ofTestable <| check.Invoke(a,b))


    let generate (spec:Machine<'Actual,'Model>) = 
        let rec genCommandsS state size =
            gen {
                if size > 0 then
                    let nextState = spec.Next state
                    let! command = nextState |> Gen.suchThat (fun command -> command.Pre state)
                    let! states, commands = genCommandsS (command.Run state) (size-1)
                    return state :: states, command :: commands
                else
                    return [state],[]
            }
        gen { let! setup = spec.Setup |> Arb.toGen
              let initialModel = setup.Model()
              let! states,commands = genCommandsS initialModel |> Gen.sized
              return { Setup = initialModel, setup
                       Operations = List.zip (List.tail states) commands //first state is actually the initial state; so drop it.
                       TearDown = spec.TearDown }
        }

    let shrink (spec:Machine<'Actual,'Model>) (run:MachineRun<_,_>) =
        let runModels initial (commands:list<_ * Operation<_,_>>) =
            let commands = commands |> List.unzip |> snd
            let newModels = commands |> List.scan (fun model operation -> operation.Run model) initial |> List.tail
            List.zip newModels commands

        let preconditionsOk (commands:List<_ * Operation<_,_>>) = 
            commands 
            |> Seq.forall (fun (model,op) -> op.Pre model)
            
        Arb.Default.FsList().Shrinker run.Operations
        //try to shrink the list of operations
        |> Seq.choose (fun commands -> 
                            let newModels = runModels (fst run.Setup) commands
                            let ok = preconditionsOk newModels
                            if ok then Some { run with Operations = newModels } else None)
        //try to srhink the initial setup state
        |> Seq.append (Arb.toShrink spec.Setup (snd run.Setup) |> Seq.map (fun create -> { run with Setup = create.Model(), create }))
        
    let check { Setup = initialModel,setup; Operations = operations; TearDown = teardown } =
        let rec run (actual,model) (cmds:list<'Model * Operation<'Actual,'Model>>) property =
            match cmds with
            | [] -> teardown.Actual actual; property
            | ((newModel,c)::cs) -> 
                let prop = c.Check(actual, newModel) |> Prop.ofTestable               
                run (actual,newModel) cs (property .&. prop)
        run (setup.Actual(), initialModel) operations (Prop.ofTestable true)

    ///Turn a specification into a property.
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let toProperty (spec:Machine<'Actual,'Model>) = 
        forAll (Arb.fromGenShrink(generate spec, shrink spec)) check
//                |> Prop.trivial (l.Length=0)
//                |> Prop.classify (l.Length > 1 && l.Length <=6) "short sequences (between 1-6 commands)" 
//                |> Prop.classify (l.Length > 6) "long sequences (>6 commands)" ))

[<AbstractClass;Sealed;Extension>]
type CommandExtensions =
    [<Extension>]
    static member ToProperty(spec: Machine<'Actual,'Model>) = Command.toProperty spec
