#light

open FSCheck.FSCheck
open FSCheck.GenReflect
open FSCheck.Shrink

quickCheck' <| (fun (x:list<list<float>>) -> List.rev x = x |>propl)