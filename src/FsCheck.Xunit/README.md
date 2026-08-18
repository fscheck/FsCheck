# FsCheck.Xunit

FsCheck.Xunit integrates FsCheck with xUnit.NET by adding a `PropertyAttribute` that runs FsCheck tests, similar to xUnit.NET's `FactAttribute`.

## Quick Start

Install via NuGet:
```
dotnet add package FsCheck.Xunit
```

Define a test class with FsCheck properties:

```fsharp
open Xunit
open FsCheck
open FsCheck.Xunit

type ListProperties =
    [<Property>]
    member _.``Reverse of reverse is identity`` (xs:list<int>) =
        List.rev(List.rev xs) = xs

    [<Property>]
    member _.``List length unchanged after reverse`` (xs:list<int>) =
        List.length (List.rev xs) = List.length xs
```

Or in C#:

```csharp
using Xunit;
using FsCheck;
using FsCheck.Xunit;

public class ListProperties
{
    [Property]
    public void ReverseOfReverseIsIdentity(int[] xs)
    {
        Assert.Equal(xs, xs.Reverse().Reverse());
    }
}
```

## Features

- **PropertyAttribute** for easy integration with xUnit.NET test discovery
- **Automatic test case generation** using FsCheck
- **Full configuration support** via attribute parameters
- **Shrinking** for minimal counter-examples

## Configuration

Configure FsCheck via the `PropertyAttribute`:

```fsharp
[<Property(MaxTest = 10000, StartSize = 100, EndSize = 1000)>]
member _.``Custom configuration`` (xs:list<int>) = true
```

## Documentation

For comprehensive documentation, visit [https://fscheck.github.io/FsCheck/](https://fscheck.github.io/FsCheck/)

## License

FsCheck.Xunit is licensed under the BSD 3-Clause license.
