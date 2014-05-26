(*** hide ***)
#I "../../src/FsCheck/bin/Release"
#r "FsCheck"

open FsCheck
open System

(**
## Grouping properties
Usually, you'll write more than one property to test. FsCheck allows you to group together properties as static members of a class: *)
type ListProperties =
  static member ``reverse of reverse is original`` (xs:list<int>) = List.rev(List.rev xs) = xs
  static member ``reverse is original`` (xs:list<int>) = List.rev xs = xs
(**These can be checked at once using:*)

(***define-output:ListProperties***)
Check.QuickAll<ListProperties>()

(**FsCheck now also prints the name of each test:*)

(***include-output:ListProperties***)

(**Since all top level functions of a a module are also compiled as static member of a class with the name of the module, 
you can also use Check.QuickAll to test all the top level functions in a certain module. 
However, the type of a module is not directly accessible via F#, so you can use the following trick:*)

(***define-output:ListProperties2***)
Check.QuickAll typeof<ListProperties>.DeclaringType

(***include-output:ListProperties2***)

