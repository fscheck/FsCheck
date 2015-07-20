(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2015 Kurt Schelfthout and contributors.              **  
**  All rights reserved.                                                    **
**  https://github.com/kurtschelfthout/FsCheck                              **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

namespace FsCheck

open System.Runtime.CompilerServices
open System.Reflection
open System.Threading

///A single command describes pre and post conditions and the model for a single method under test.
///The post-conditions are the invariants that will be checked; when these do not hold the test fails.
[<AbstractClass>]
type Command<'Actual,'Model>() =
    ///Excecutes the command on the object under test, and returns a property that must hold.
    ///This property typically compares the state of the model with the state of the object after
    ///execution of the command.
    abstract Check : 'Actual * 'Model  -> Property
    ///Executes the command on the model of the object.
    abstract RunModel : 'Model -> 'Model
    ///Optional precondition for execution of the command. When this does not hold, the test continues
    ///but the command is not executed.
    abstract Pre : 'Model -> bool
    ///The default precondition is true.
    default __.Pre _ = true

[<AbstractClass>]
type Create<'Actual,'Model>() =
    ///Randomly generate the initial state of the actual object. Should still correspond to the 
    ///initial state of model object; so you should only randomly generate parameters to the instance
    ///that don't affect the model.
    ///Note:make sure that each value is truly a new instance if the commands change the state
    ///of the object. Gen.connstant in particular is a bad idea - use Gen.fresh instead.
    abstract Actual : unit -> 'Actual
    ///Initial state of model object. Must correspond to initial state of actual object.
    abstract Model : unit -> 'Model
    override __.ToString() = sprintf "Create %s" typeof<'Actual>.Name

type Destroy<'Actual>() =
    abstract Actual : 'Actual -> unit
    default __.Actual _ = ()
    override __.ToString() = sprintf "Destroy %s" typeof<'Actual>.Name

///Defines the initial state for actual and model object, and allows to define the generator to use
///for the next state, based on the model.
[<AbstractClass>]
type CommandGenerator<'Actual,'Model>() =
    abstract Create : Arbitrary<Create<'Actual,'Model>>
    abstract Destroy : Destroy<'Actual>
    default __.Destroy = Destroy<_>()
    ///Generate a number of possible commands based on the current state of the model. 
    ///Preconditions are still checked, so even if a Command is returned, it is not chosen
    ///if its precondition does not hold.
    abstract Next : 'Model -> Gen<Command<'Actual,'Model>>


//a single assignment variable
type CommandResult(?result) =
    static let mutable resultCounter = 0
    static let nextCounter() = Interlocked.Increment &resultCounter
    let mutable counter = nextCounter()
    let mutable result = result
    member __.Get = result
    member __.Set v = match result with None -> result <- Some v | Some _ -> invalidOp "Result already set to value."
    override __.ToString() = sprintf "result_%i" counter

type ReflectiveModel = list<CommandResult*string> //a list of commands as string?

type ReflectiveCreate<'Actual>(ctor:ConstructorInfo, parameters:array<obj>) =
    inherit Create<'Actual,ReflectiveModel>()
    override __.Actual() = ctor.Invoke parameters  :?> 'Actual
    override __.Model() = 
        let result = CommandResult()
        let str = parameters 
                  |> Seq.map (fun p -> p.ToString()) 
                  |> String.concat ", " 
                  |> sprintf "let %O = new %s(%s)" result ctor.Name
        [ result,str ]

type ReflectiveCommand<'Actual>(meth:MethodInfo, parameters:array<obj>) =
    inherit Command<'Actual,ReflectiveModel>()
    let paramstring = 
                  parameters
                  |> Seq.map (fun p -> p.ToString()) 
                  |> String.concat ", " 
    override __.RunModel model =
        let result = CommandResult()
        let last = Seq.last model |> fst
        let str = sprintf "let %O = %O.%s(%s)" last result meth.Name paramstring
        (result,str) :: model
    override __.Check(actual, model) =
        //invoke on actual
        //what to test? how to get the property?
        //separate input? based on function - needs to wrap invocation to catch exceptions?
        //or some DSL like thnigs - but property is already that...so try to reuse
        // fun model invoker -> try let res = invoker();  with 
        ///should model be a quotation/expression so we can match?
        let result = lazy let result = meth.Invoke(actual, parameters)
                          let modelResult = Seq.last model |> fst
                          modelResult.Set result
        Prop.ofTestable result
    override __.ToString() =
        sprintf "%s(%s)" meth.Name paramstring

type ReflectiveCommandGenerator<'Actual>() = 
    inherit CommandGenerator<'Actual,ReflectiveModel>()
    let parameterGenerator (parameters:seq<ParameterInfo>) =
        parameters |> Seq.map (fun p -> p.ParameterType) |> Seq.map Arb.getGenerator |> Gen.sequence
    let ctors = 
        typeof<'Actual>.GetConstructors() 
        |> Seq.map (fun ctor -> 
                        gen { let! parameters = parameterGenerator (ctor.GetParameters())
                              return ReflectiveCreate<'Actual>(ctor, List.toArray parameters) :> Create<'Actual,ReflectiveModel> })
        |> Seq.toArray
        |> Gen.oneof

    let instanceMethods =
        typeof<'Actual>.GetMethods(BindingFlags.Public ||| BindingFlags.Instance)
        |> Seq.map (fun meth -> 
                        gen { let! parameters = parameterGenerator (meth.GetParameters())
                              return ReflectiveCommand<'Actual>(meth, List.toArray parameters) :> Command<'Actual,ReflectiveModel> })
        |> Seq.toArray
        |> Gen.oneof

    override __.Create = ctors |> Arb.fromGen
    override __.Next model = instanceMethods

module Command =
    open System
    open System.ComponentModel
    open Prop

    [<CompiledName("Create"); EditorBrowsable(EditorBrowsableState.Never)>]
    let create actual model =
        { new Create<_,_>() with
            override __.Actual() = actual()
            override __.Model() = model() }

    [<CompiledName("Create"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let createFunc (actual:Func<_>) (model:Func<_>) =
        { new Create<_,_>() with
            override __.Actual() = actual.Invoke()
            override __.Model() = model.Invoke() }

    [<CompiledName("Destroy"); EditorBrowsable(EditorBrowsableState.Never)>]
    let destroy run =
        { new Destroy<_>() with
            override __.Actual actual = run actual }

    [<CompiledName("Destroy"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let destroyAction (run:Action<_>) =
        { new Destroy<_>() with
            override __.Actual actual = run.Invoke actual }

    [<CompiledName("FromFun"); EditorBrowsable(EditorBrowsableState.Never)>]
    let fromFunWithPrecondition name preCondition runModel check =
        { new Command<'Actual,'Model>() with
            override __.RunModel pre = runModel pre
            override __.Check(model,actual) = check (model,actual) |> Prop.ofTestable
            override __.Pre model = preCondition model
            override __.ToString() = name }

    [<CompiledName("FromFun"); EditorBrowsable(EditorBrowsableState.Never)>]
    let fromFun name runModel check =
        { new Command<'Actual,'Model>() with
            override __.RunModel pre = runModel pre
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


    let generate (spec:CommandGenerator<'Actual,'Model>) = 
        let rec genCommandsS state size =
            gen {
                if size > 0 then
                    let! command = spec.Next state |> Gen.suchThat (fun command -> command.Pre state)
                    let! commands = genCommandsS (command.RunModel state) (size-1)
                    return command :: commands
                else
                    return []
            }
        gen { let! create = spec.Create |> Arb.toGen
              let! commands = genCommandsS (create.Model()) |> Gen.sized
              return (create, commands, spec.Destroy)
        }

    let shrink (spec:CommandGenerator<'Actual,'Model>) (create:Create<'Actual,'Model>, commands:list<Command<'Actual,'Model>>, destroy:Destroy<'Actual>) =
        let preconditionsOk initial (commands:seq<Command<_,_>>) = 
            commands 
            |> Seq.fold (fun (model,pres) cmd -> cmd.RunModel model,pres && cmd.Pre model) (initial, true)
            |> snd
        Arb.Default.FsList().Shrinker commands
        |> Seq.choose (fun commands -> if preconditionsOk (create.Model()) commands then Some (create,commands,destroy) else None)
        |> Seq.append (Arb.toShrink spec.Create create |> Seq.map (fun create -> (create,commands,destroy)))
        
    let check (create:Create<'Actual,'Model>, commands:list<Command<'Actual,'Model>>, destroy:Destroy<'Actual>) =
        let rec run (actual,model) (cmds:list<Command<'Actual,'Model>>) property =
            match cmds with
            | [] -> destroy.Actual actual; property
            | (c::cs) -> 
                let newModel = c.RunModel model
                let prop = c.Check(actual, newModel) |> Prop.ofTestable               
                run (actual,newModel) cs (property .&. prop)
        run (create.Actual(), create.Model()) commands (Prop.ofTestable true)

    ///Turn a specification into a property.
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let toProperty (spec:CommandGenerator<'Actual,'Model>) = 
        forAll (Arb.fromGenShrink(generate spec, shrink spec)) check
//                |> Prop.trivial (l.Length=0)
//                |> Prop.classify (l.Length > 1 && l.Length <=6) "short sequences (between 1-6 commands)" 
//                |> Prop.classify (l.Length > 6) "long sequences (>6 commands)" ))

[<AbstractClass;Sealed;Extension>]
type CommandExtensions =
    [<Extension>]
    static member ToProperty(spec: CommandGenerator<'Actual,'Model>) = Command.toProperty spec
