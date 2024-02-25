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

Warning: Documentation Problems
-------------------------------

The Documentation section was written for version FsCheck 2.x, but 3.x is the version that is actually maintained at this point and will get new features and fixes (even though it's in pre-release).

This leaves us in the unfortunate position that the section under the next heading are out of date and incomplete. The API docs are generated from the 3.x code and so are up to date - if in doubt, believe the API docs.

The documentation and API docs for 2.x are not easily accessible anymore, sadly. The last commit of the 2.x documentation site is [here](https://github.com/fscheck/FsCheck/tree/1458b268b4311f7e4b25871715f1f9b5d58a21b3).

FsCheck could use your contributions to docs (or otherwise!):

* See [issues](https://github.com/fscheck/FsCheck/issues?q=is%3Aissue+is%3Aopen+label%3A%22help+wanted%22) for inspiration.
* Open an issue that highlights specific documentation problems or gaps (even if you're not sure it really is a problem - it'll highlight confusion)

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
