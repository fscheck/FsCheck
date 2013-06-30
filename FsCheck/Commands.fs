(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2013 Kurt Schelfthout. All rights reserved.          **
**  https://github.com/kurtschelfthout/FsCheck                              **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

#light

namespace FsCheck

module Commands =

    open Arb
    open Prop

    ///A single command describes pre and post conditions and the model for a single method under test.
    [<AbstractClass>]
    type ICommand<'o,'s>() =
        ///Excecutes the command on the actual object under test.
        abstract RunActual : 'o -> 'o
        ///Executes the command on the model of the object.
        abstract RunModel : 's -> 's
        ///Precondition for execution of the command.
        abstract Pre : 's -> bool
        ///Postcondition that must hold after execution of the command.
        abstract Post : 'o * 's -> bool //(*Testable 'a*)
        default x.Pre _ = true
        default x.Post (_,_) = true

    ///A specification for an object under test, based on an abstract model of the
    ///object's behavior.
    type ISpecification<'o,'s> =
        ///Initial state of both object and model.
        abstract Initial : unit -> 'o * 's
        ///Generate a number of possible commands based on the current state of the model. The commands
        ///are just hints to speed up execution; preconditions are still checked.
        abstract GenCommand : 's -> Gen<ICommand<'o,'s>>

    let private genCommands (spec:ISpecification<_,_>) = 
        let rec genCommandsS state size =
            gen {
                if size > 0 then
                    let! command = (spec.GenCommand state) |> Gen.suchThat (fun command -> command.Pre state)
                    let! commands = genCommandsS (command.RunModel state) (size-1)
                    return (command :: commands)
                else
                    return []
            }
            |> Gen.map List.rev
        Gen.sized <| genCommandsS (spec.Initial() |> snd)      
     
    ///Turn a specification into a property.
    let asProperty (spec:ISpecification<_,_>) =
        let rec applyCommands (actual,model) (cmds:list<ICommand<_,_>>) =
            match cmds with
            | [] -> Testable.property true
            | (c::cs) -> 
                c.Pre model ==> 
                    lazy (let newActual = c.RunActual actual
                          let newModel = c.RunModel model
                          c.Post (newActual,newModel) .&. applyCommands (newActual,newModel) cs)
        forAll (Arb.fromGenShrink(genCommands spec,shrink)) (fun l -> l |> applyCommands (spec.Initial()))
        