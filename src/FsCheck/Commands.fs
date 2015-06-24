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
    abstract Create : Gen<Create<'Actual,'Model>>
    abstract Destroy : Destroy<'Actual>
    default __.Destroy = Destroy<_>()
    ///Generate a number of possible commands based on the current state of the model. 
    ///Preconditions are still checked, so even if a Command is returned, it is not chosen
    ///if its precondition does not hold.
    abstract Next : 'Model -> Gen<Command<'Actual,'Model>>

module Command =
    open System
    open System.ComponentModel
    open Arb
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
    let destroyFunc (run:Action<_>) =
        { new Destroy<_>() with
            override __.Actual actual = run.Invoke actual }

    [<CompiledName("FromFun"); EditorBrowsable(EditorBrowsableState.Never)>]
    let fromFunWithPrecondition preCondition runModel check =
        { new Command<'Actual,'Model>() with
            override __.RunModel pre = runModel pre
            override __.Check(model,actual) = check (model,actual) |> Prop.ofTestable
            override __.Pre model = preCondition model}

    [<CompiledName("FromFun"); EditorBrowsable(EditorBrowsableState.Never)>]
    let fromFun runModel check =
        { new Command<'Actual,'Model>() with
            override __.RunModel pre = runModel pre
            override __.Check(model,actual) = check (model,actual) |> Prop.ofTestable }

    [<CompiledName("FromFunc"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let fromFuncProp<'Actual,'Model> (runModel:Func<'Model,_>) (check:Func<'Actual,_,Property>) =
        fromFun runModel.Invoke (fun (a,b) -> check.Invoke(a,b))

    [<CompiledName("FromFunc"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let fromFuncBool<'Actual,'Model> (runModel:Func<'Model,_>) (check:Func<'Actual,_,bool>) =
        fromFun runModel.Invoke (fun (a,b) -> Prop.ofTestable <| check.Invoke(a,b))

    [<CompiledName("FromFunc"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let fromFuncAction<'Actual,'Model> (runModel:Func<'Model,_>) (check:Action<'Actual,_>) =
        fromFun runModel.Invoke (fun (a,b) -> Prop.ofTestable <| check.Invoke(a,b))


    let generator (spec:CommandGenerator<'Actual,'Model>) = 
        let rec genCommandsS state size =
            gen {
                if size > 0 then
                    let! command = spec.Next state |> Gen.suchThat (fun command -> command.Pre state)
                    let! commands = genCommandsS (command.RunModel state) (size-1)
                    return command :: commands
                else
                    return []
            }
        gen { let! create = spec.Create
              let! commands = genCommandsS (create.Model()) |> Gen.sized
              return (create, commands, spec.Destroy)
        }
     
    ///Turn a specification into a property.
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let toProperty (spec:CommandGenerator<'Actual,'Model>) =
        let rec run (actual,model) (cmds:list<Command<'Actual,'Model>>) property =
            match cmds with
            | [] -> property
            | (c::cs) -> 
                let newModel = c.RunModel model
                let prop = c.Check(actual, newModel) |> Prop.ofTestable               
                run (actual,newModel) cs (property .&. prop)
        
        forAll (Arb.fromGen(generator spec)) (fun (create, commands, destroy) -> 
            run (create.Actual(), create.Model()) commands (Prop.ofTestable true))
//                |> Prop.trivial (l.Length=0)
//                |> Prop.classify (l.Length > 1 && l.Length <=6) "short sequences (between 1-6 commands)" 
//                |> Prop.classify (l.Length > 6) "long sequences (>6 commands)" ))

[<AbstractClass;Sealed;Extension>]
type CommandExtensions =
    [<Extension>]
    static member ToProperty(spec: CommandGenerator<'Actual,'Model>) = Command.toProperty spec
