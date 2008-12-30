#light

namespace FSCheck

open System
open System.Reflection
open FSCheck.Reflect

module Common =

    let genericTypeEq (lhs: System.Type) (rhs: System.Type) : bool =
        lhs.IsGenericType && rhs.IsGenericType &&
            (lhs.GetGenericTypeDefinition() = rhs.GetGenericTypeDefinition())



    // Compute which types are possible children of this type
    // Helps make union generation terminate quicker
    let containedTypes (t : Type) : List<Type> =
        [] // TODO
