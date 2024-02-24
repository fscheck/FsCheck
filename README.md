
# What is FsCheck? #

FsCheck is a tool for testing .NET programs automatically. The programmer provides a specification of the program, in the form of properties that functions, methods or objects should satisfy, and FsCheck then tests that the properties hold in a large number of randomly generated cases. While writing the properties, you are writing a testable specification of your program. Specifications are expressed in F#, C# or VB, using combinators defined in the FsCheck library. FsCheck provides combinators to define properties, observe the distribution of test data, and define test data generators. When a property fails, FsCheck automatically displays a minimal counter-example.

FsCheck is a port of Haskell's [QuickCheck](http://www.cse.chalmers.se/~rjmh/QuickCheck/). Important parts of the [manual](https://fscheck.github.io/FsCheck/) for using FsCheck are almost literally adapted from the QuickCheck [manual](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html). Any errors and omissions are entirely my responsibility.

Since v0.5, [scalacheck](https://github.com/rickynils/scalacheck) has influenced FsCheck as well. Scalacheck is itself a port of QuickCheck to Scala.

FsCheck's generator combinators can be used in any testing framework to easily generate random values for many types, and FsCheck itself integrates nicely with existing unit testing frameworks such as NUnit, xUnit, MSTest and MbUnit.

# Releases #

* [FsCheck](http://nuget.org/List/Packages/FsCheck)
* [FsCheck.Xunit](http://nuget.org/List/Packages/FsCheck.Xunit)
* [FsCheck.Nunit](http://www.nuget.org/packages/FsCheck.Nunit/)

FsCheck follows [Semantic Versioning 2.0.0](http://semver.org/spec/v2.0.0.html), except for the API exposed in `FsCheck.Experimental` which is subject to change at any time.

All AppVeyor builds are available using the NuGet feed: <https://ci.appveyor.com/nuget/fscheck>

If using Paket, add the source at the top of `paket.dependencies`.

```paket
source https://www.nuget.org/api/v2
source https://ci.appveyor.com/nuget/fscheck
```

See the build history for a list of available versions: <https://ci.appveyor.com/project/kurtschelfthout/fscheck/history>

Here are some options for specifying the dependency:

```paket
nuget FsCheck
nuget FsCheck prerelease
nuget FsCheck 2.0.4
nuget FsCheck 2.0.5-b247
```

# Documentation #

* [English](https://fscheck.github.io/FsCheck/)

# Contributing #

Pull requests are very welcome!

Check out the issues marked "good first issue" and "help wanted" if you need any inspiration.

We rarely reject PRs. If you intend to make a bigger change, it's better to open an issue first to discuss.

## Building ##

Check out the project and run build.cmd on Windows or build.sh on Linux/OSX. That should pull in all the dependencies, build and run the tests.

For Visual Studio/MonoDevelop/Xamarin Studio/VsCode: open (the folder that contains) FsCheck.sln and start coding.

FsCheck uses a build script inspired by FAKE. Run `build.[cmd|sh] -t <Target>` (or `dotnet fsi build.fsx -t <Target>`) to do something. Important targets are:

* `Build`: cleanly builds all platforms in Release mode.
* `Tests`: builds and runs the tests.
* `Docs`:  builds and generates documentation. FsCheck uses FSharp.Formatting, so literate fsx files in the docs folder.
* `WatchDocs`: convenient when developing documentation - starts a local webserver and watches for changes in the docs folder.
* `NuGetPack`: Creates NuGet packages.
* `CI`: Target that is run on AppVeyor, basically all of the above.

## CI ##

AppVeyor [![Build status](https://ci.appveyor.com/api/projects/status/7ytaslpgxxtw7036/branch/master)](https://ci.appveyor.com/project/kurtschelfthout/fscheck)
