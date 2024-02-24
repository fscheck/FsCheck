(**
FsCheck: Random Testing for .NET
================================

FsCheck is a tool for testing .NET programs automatically. The programmer provides a specification of the program, in the form of properties which functions, methods or objects should satisfy, and FsCheck then tests that the properties hold in a large number of randomly generated cases. While writing the properties, you are actually writing a testable specification of your program. Specifications are expressed in F#, C# or VB, using combinators defined in the FsCheck library. FsCheck provides combinators to define properties, observe the distribution of test data, and define test data generators. When a property fails, FsCheck automatically displays a minimal counter example.

FsCheck, NUnit and xUnit.NET plugin can be <a href="https://www.nuget.org/packages?q=fscheck">installed from NuGet</a> using your favorite package manager.

Users have also created integrations with [Expecto](https://github.com/haf/Expecto), 
[Fuchu](https://github.com/mausch/Fuchu), 
[AutoFixture](https://github.com/AutoFixture/AutoFixture) and 
[MSTest](https://github.com/microsoft/testfx) that 
you can [find on NuGet](https://www.nuget.org/packages?q=fscheck).

Documentation
-------------

 * [QuickStart](QuickStart.html) to get started.

 * [Properties](Properties.html) describes FsCheck's language to express tests - 
   in other frameworks these are often called parametrized tests or
   generative tests. FsCheck calls them properties.

 * [Generating test data](TestData.html) describes how to guide FsCheck to
   generate better data or to stop it from generating data that doesn't make
   sense for what you're trying to test. FsCheck has a flexible language to describe test 
   value generators and shrinkers, and apply them to your properties.

 * [Running Tests](RunningTests.html) explains various ways to run FsCheck tests and how to
   integrate with unit testing frameworks.

 * [Model based testing](StatefulTestingNew.html), for testing stateful systems and objects. Since this is in the Experimental namespace, semantic versioning promises do not apply to this part of the API.

 * [Tips and tricks](TipsAndTricks.html) 

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions.


 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding new public API, please also 
consider adding [samples][content] that can be turned into documentation.

The library is available under the BSD license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/fscheck/FsCheck/tree/master/docs
  [gh]: https://github.com/fscheck/FsCheck
  [issues]: https://github.com/fscheck/FsCheck/issues
  [readme]: https://github.com/fscheck/FsCheck/blob/master/README.md
  [license]: https://github.com/fscheck/FsCheck/blob/master/License.txt
*)
