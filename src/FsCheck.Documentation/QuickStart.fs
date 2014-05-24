module QuickStart

open FsCheck

let revRevIsOrig (xs:list<int>) = List.rev(List.rev xs) = xs
Check.Quick revRevIsOrig
let revIsOrig (xs:list<int>) = List.rev xs = xs
Check.Quick revIsOrig

type ListProperties =
    static member ``reverse of reverse is original`` xs = revRevIsOrig xs
    static member ``reverse is original`` xs = revIsOrig xs
Check.QuickAll<ListProperties>()

Check.QuickAll typeof<ListProperties>.DeclaringType

let revRevIsOrigFloat (xs:list<float>) = List.rev(List.rev xs) = xs
Check.Quick revRevIsOrigFloat