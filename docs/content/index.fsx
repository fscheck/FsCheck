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
      FsCheck and xUnit.NET plugin can be <a href="https://nuget.org/packages/FsCheck">installed from NuGet</a>:
      <pre>PM> Install-Package FsCheck
           PM> Install-Package FsCheck.Xunit</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

Example
-------

This example demonstrates using a function defined in this sample library.

*)
#r "FsCheck.dll"
open FsCheck

//printfn "hello = %i" <| Library.hello 0

(**
Some more info

Samples & documentation
-----------------------

The library comes with comprehensive documentation. 
It can include a tutorials automatically generated from `*.fsx` files in [the content folder][content]. 
The API reference is automatically generated from Markdown comments in the library implementation.

 * [QuickStart](QuickStart.html) contains a further explanation of this sample library.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding new public API, please also 
consider adding [samples][content] that can be turned into documentation. You might
also want to read [library design notes][readme] to understand how it works.

The library is available under the BSD license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/fsharp/FsCheck/tree/master/docs/content
  [gh]: https://github.com/fsharp/FsCheck
  [issues]: https://github.com/fsharp/FsCheck/issues
  [readme]: https://github.com/fsharp/FsCheck/blob/master/README.md
  [license]: https://github.com/fsharp/FsCheck/blob/master/LICENSE.txt
*)
