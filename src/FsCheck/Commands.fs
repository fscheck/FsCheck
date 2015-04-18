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
    ///Excecutes the command on the actual object under test.
    abstract RunActual : 'Actual -> 'Actual
    ///Executes the command on the model of the object.
    abstract RunModel : 'Model -> 'Model
    ///Precondition for execution of the command. When this does not hold, the test continues
    ///but the command will not be executed.
    abstract Pre : 'Model -> bool
    ///Postcondition that must hold after execution of the command. Compares state of model and actual
    ///object and fails the property if they do not match.
    abstract Post : 'Actual * 'Model -> Property
    ///The default precondition is true.
    default __.Pre _ = true
    ///The default postcondition is true.
    default __.Post (_,_) = Prop.ofTestable true

///Defines the initial state for actual and model object, and allows to define the generator to use
///for the next state, based on the model.
type ICommandGenerator<'Actual,'Model> =
    ///Initial state of actual object. Should correspond to initial state of model object.
    abstract InitialActual : 'Actual
    ///Initial state of model object. Should correspond to initial state of actual object.
    abstract InitialModel : 'Model
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
    let create preCondition runActual runModel checkPostCondition =
        { new Command<'Actual,'Model>() with
            override __.RunActual pre = runActual pre
            override __.RunModel pre = runModel pre
            override __.Post(model,actual) = checkPostCondition (model,actual)
            override __.Pre model = defaultArg preCondition (fun _ -> true) model}

//    [<CompiledName("Create"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
//    let createFunc (runActual:Func<'Actual,_>) (runModel:Func<'Model,_>) (postCondition:Func<_,_,Specification>) =
//        create None runActual.Invoke runModel.Invoke (fun (a,b) -> postCondition.Invoke(a,b).Build())

//    [<CompiledName("Create"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
//    let createPreFunc (preCondition:Func<_,_>) (runActual:Func<'Actual,_>) (runModel:Func<'Model,_>) (postCondition:Func<_,_,Specification>) =
//        create (Some preCondition.Invoke) runActual.Invoke runModel.Invoke (fun (a,b) -> postCondition.Invoke(a,b).Build())

    let private genCommands (spec:ICommandGenerator<_,_>) = 
        let rec genCommandsS state size =
            gen {
                if size > 0 then
                    let! command = spec.Next state |> Gen.suchThat (fun command -> command.Pre state)
                    let! commands = genCommandsS (command.RunModel state) (size-1)
                    return command :: commands
                else
                    return []
            }
            |> Gen.map List.rev
        spec.InitialModel |> genCommandsS |> Gen.sized
     
    ///Turn a specification into a property.
    [<CompiledName("ToProperty"); EditorBrowsable(EditorBrowsableState.Never)>]
    let toProperty (spec:ICommandGenerator<'Actual,'Model>) =
        let rec applyCommands (actual,model) (cmds:list<Command<_,_>>) =
            match cmds with
            | [] -> Testable.property true
            | (c::cs) -> 
                c.Pre model ==> 
                    lazy (let newActual = c.RunActual actual
                          let newModel = c.RunModel model
                          c.Post (newActual,newModel) .&. applyCommands (newActual,newModel) cs)
                
        forAll (Arb.fromGenShrink(genCommands spec,shrink))  //note: this uses the list shrinker which is not correct - should take preconditions into accout for example
                (fun l -> l |> applyCommands (spec.InitialActual, spec.InitialModel) 
                            |> Prop.trivial (l.Length=0)
                            |> Prop.classify (l.Length > 1 && l.Length <=6) "short sequences (between 1-6 commands)" 
                            |> Prop.classify (l.Length > 6) "long sequences (>6 commands)" )
