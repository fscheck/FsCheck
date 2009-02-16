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

module Commands

[<AbstractClass>]
type ICommand<'o,'s> =
    abstract RunActual : 'o -> 'o
    abstract RunModel : 's -> 's
    abstract Pre : 's -> bool
    abstract Post : 's * 'o -> bool
    default x.Pre _ = true
    default x.Post (_,_) = true

type ISpecification<'o,'s> =
    abstract Initial : unit -> 's * 'o
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
    sized <| genCommandsS (spec.Initial() |> fst)      

let propCommand (spec:ISpecification<_,_>) = ()