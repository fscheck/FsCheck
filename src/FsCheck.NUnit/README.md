# FsCheck.NUnit

FsCheck.NUnit integrates FsCheck with NUnit by adding a `FsCheckPropertyAttribute` that runs FsCheck tests, similar to NUnit's `TestAttribute`.

## Quick Start

Install via NuGet:
```
dotnet add package FsCheck.NUnit
```

Define a test class with FsCheck properties:

```fsharp
open NUnit.Framework
open FsCheck
open FsCheck.NUnit

[<TestFixture>]
type ListProperties() =
    [<FsCheckProperty>]
    member _.``Reverse of reverse is identity`` (xs:list<int>) =
        List.rev(List.rev xs) = xs

    [<FsCheckProperty>]
    member _.``List length unchanged after reverse`` (xs:list<int>) =
        List.length (List.rev xs) = List.length xs
```

Or in C#:

```csharp
using NUnit.Framework;
using FsCheck;
using FsCheck.NUnit;

[TestFixture]
public class ListProperties
{
    [FsCheckProperty]
    public void ReverseOfReverseIsIdentity(int[] xs)
    {
        Assert.AreEqual(xs, xs.Reverse().Reverse());
    }
}
```

## Features

- **FsCheckPropertyAttribute** for easy integration with NUnit test discovery
- **Automatic test case generation** using FsCheck
- **Full configuration support** via attribute parameters
- **Shrinking** for minimal counter-examples

## Configuration

Configure FsCheck via the `FsCheckPropertyAttribute`:

```fsharp
[<FsCheckProperty(MaxTest = 10000, StartSize = 100, EndSize = 1000)>]
member _.``Custom configuration`` (xs:list<int>) = true
```

## Documentation

For comprehensive documentation, visit [https://fscheck.github.io/FsCheck/](https://fscheck.github.io/FsCheck/)

## License

FsCheck.NUnit is licensed under the BSD 3-Clause license.
