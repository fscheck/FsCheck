# FsCheck

FsCheck is a tool for testing .NET programs automatically. The programmer provides a specification of the program in the form of properties which functions, methods or objects should satisfy, and FsCheck then tests that the properties hold in a large number of randomly generated cases.

## Quick Start

Install via NuGet:
```
dotnet add package FsCheck
```

Define a property as a function that returns `bool`:

```fsharp
open FsCheck

let revRevIsOrig (xs:list<int>) = List.rev(List.rev xs) = xs

Check.Quick revRevIsOrig
```

When a property fails, FsCheck automatically displays a minimal counter-example and shrinks it to find the simplest failure case.

## Features

- **Automatic test case generation** using property-based testing
- **Shrinking** to find minimal counter-examples when tests fail
- **Composable generators** to guide test data generation
- **Integration** with NUnit, xUnit.NET, MSTest and other frameworks
- **Works with F#, C# and VB.NET**

## Documentation

For comprehensive documentation, visit [https://fscheck.github.io/FsCheck/](https://fscheck.github.io/FsCheck/)

- [Quick Start Guide](https://fscheck.github.io/FsCheck/QuickStart.html)
- [Writing Properties](https://fscheck.github.io/FsCheck/Properties.html)
- [Generating Test Data](https://fscheck.github.io/FsCheck/TestData.html)
- [Running Tests](https://fscheck.github.io/FsCheck/RunningTests.html)

## License

FsCheck is licensed under the BSD 3-Clause license.
