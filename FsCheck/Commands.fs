(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2009 Kurt Schelfthout. All rights reserved.          **
**  http://www.codeplex.com/fscheck                                         **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

#light

namespace FsCheck

[<AutoOpen>]
module Commands

[<AbstractClass>]
type ICommand<'o,'s>() =
    abstract RunActual : 'o -> 'o
    abstract RunModel : 's -> 's
    abstract Pre : 's -> bool
    abstract Post : 'o * 's -> bool //(*Testable 'a*)
    default x.Pre _ = true
    default x.Post (_,_) = true

type ISpecification<'o,'s> =
    abstract Initial : unit -> 'o * 's
    abstract GenCommand : 's -> Gen<ICommand<'o,'s>>

let genCommands (spec:ISpecification<_,_>) = 
    let rec genCommandsS state size =
        gen {
            if size > 0 then
                let! command = (spec.GenCommand state) |> suchThat (fun command -> command.Pre state)
                let! commands = genCommandsS (command.RunModel state) (size-1)
                return (command :: commands)
            else
                return []
        }
        |> fmapGen List.rev
    sized <| genCommandsS (spec.Initial() |> snd)      

//let shrinkCommands cmds =
    

let propCommand (spec:ISpecification<_,_>) =
    let rec applyCommands (actual,model) (cmds:list<ICommand<_,_>>) =
        match cmds with
        | [] -> property true
        | (c::cs) -> 
            c.Pre model ==> 
                let newActual = c.RunActual actual
                let newModel = c.RunModel model
                c.Post (newActual,newModel) .&. applyCommands (newActual,newModel) cs
    forAllShrink (genCommands spec) shrink (fun l -> l |> applyCommands (spec.Initial()))
    