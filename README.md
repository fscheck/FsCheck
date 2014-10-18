### What is FsCheck? ###

FsCheck is a tool for testing .NET programs automatically. The programmer provides a specification of the program, in the form of properties which functions, methods or objects should satisfy, and FsCheck then tests that the properties hold in a large number of randomly generated cases. While writing the properties, you are actually writing a testable specification of your program. Specifications are expressed in F#, C# or VB, using combinators defined in the FsCheck library. FsCheck provides combinators to define properties, observe the distribution of test data, and define test data generators. When a property fails, FsCheck automatically displays a minimal counter example.

FsCheck is a port of Haskell's [QuickCheck](http://www.cse.chalmers.se/~rjmh/QuickCheck/). Important parts of the manual for using FsCheck is almost literally adapted from the QuickCheck [manual](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html). Any errors and omissions are entirely my responsibility.

Since v0.5, [scalacheck](https://github.com/rickynils/scalacheck) has influenced FsCheck as well. Scalacheck is itself a port of QuickCheck to Scala.

FsCheck's generator combinators can be used in any testing framework to easily generate a number of random values for many types, and FsCheck itself integrates nicely with existing unit testing frameworks such as NUnit, xUnit, MSTest and MbUnit.

### NuGet ###

* [FsCheck](http://nuget.org/List/Packages/FsCheck)
* [FsCheck with xUnit.NET integration](http://nuget.org/List/Packages/FsCheck.Xunit)
* [FsCheck with NUnit integration](http://www.nuget.org/packages/FsCheck.Nunit/)
 
### Documentation ###

* [English](https://fsharp.github.io/FsCheck/)
* [Japanese](https://fsharp.github.io/FsCheck/ja)

### More reading ###
* [Blog](http://fortysix-and-two.blogspot.com) for announcements, change logs and other posts more or less related to FsCheck. 
* An interesting read about QuickCheck is in [Chapter 11](http://book.realworldhaskell.org/read/testing-and-quality-assurance.html) of the excellent [Real world Haskell](http://book.realworldhaskell.org) book. The book is freely available online, but do the authors a favor and buy it, it's really worth it  also for F# programmers.

### Contributing ###

Pull requests very welcome! Check out the issues marked up-for-grabs if you need any inspiration.

Checkout the project and run build.cmd on Windows or build.sh on Linux/OSX. That should pull in all the dependencies, build, run the tests and generate the documentation.

Then, open FsCheck.sln in Visual Studio on Windows or FsCheck-mono.sln on Linux/OSX in and start coding or writing documentation. 

Building and testing from Visual Studio also works, to re-generate documentation quickly, execute docs/tools/generate.fsx in FSI. This should also work in Monodevelop and Xamarin, though I have not tested.

- Windows on .NET:[![Build status](https://ci.appveyor.com/api/projects/status/7ytaslpgxxtw7036)](https://ci.appveyor.com/project/kurtschelfthout/fscheck)
- OSX on Mono: [![Build Status](https://travis-ci.org/fsharp/FsCheck.svg?branch=master)](https://travis-ci.org/fsharp/FsCheck)



