(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../src/FsCheck/bin/Release"

(**
FsCheck
=======

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      FsCheck, NUnit and xUnit.NET plugin can be <a href="https://www.nuget.org/packages?q=fscheck">installed from NuGet</a>:
      <pre>PM> Install-Package FsCheck
PM> Install-Package FsCheck.NUnit
PM> Install-Package FsCheck.Xunit</pre>
    </div>
  </div> 
  <div class="span1"></div>
</div>

Users have also created integrations with [Expecto](https://github.com/haf/Expecto), 
[Fuchu](https://github.com/mausch/Fuchu) and 
[AutoFixture](https://github.com/AutoFixture/AutoFixture) that 
you can [find on NuGet](https://www.nuget.org/packages?q=fscheck).

Documentation
-------------

The documentation is also available in [Japanese](ja/index.html).

 * [QuickStart](QuickStart.html) to get started.

 * [Properties](Properties.html) descibes FsCheck's language to express tests - 
   in other frameworks these are often called parametrized tests or
   generative tests. FsCheck calls them properties.

 * [Generating test data](TestData.html) describes how to guide FsCheck to
   generate better data or to stop it from generating data that doesn't make
   sense for what you're trying to test. FsCheck has a flexible language to describe test 
   value generators and shrinkers, and apply them to your properties.

 * [Model based testing](StatefulTesting.html) is a particular testing approach
   where FsCheck generates a large number of random operations on an object or data
   structure, and the results of each operation are compared with a (much simpler)
   model.

 * [Running Tests](RunningTests.html) explains various ways to run FsCheck tests and how to
   integrate with unit testing frameworks.

 * [Tips and tricks](TipsAndTricks.html) 

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions.

 * [Model based testing vNext (Experimental)](StatefulTestingNew.html) is a relatively new addition for
   a more powerful model based testing. It's still a bit rough around the edges, some things may not
   work, and it is subject to change (i.e. no promises regarding semantic versioning and breaking changes). Regardless,
   feedback is very much appreciated!
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding new public API, please also 
consider adding [samples][content] that can be turned into documentation.

The library is available under the BSD license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/fscheck/FsCheck/tree/master/docs/content
  [gh]: https://github.com/fscheck/FsCheck
  [issues]: https://github.com/fscheck/FsCheck/issues
  [readme]: https://github.com/fscheck/FsCheck/blob/master/README.md
  [license]: https://github.com/fscheck/FsCheck/blob/master/License.txt
*)
