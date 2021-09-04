### 2.16.3 - 4 September 2021

* Allow configuration in FsCheck.Xunit.PropertiesAttribute to affect properties on nested types or modules. PropertiesAttribute on the closest enclosing type takes precedence.

* Add FsCheck.Xunit.TestOutputRunner - a simple runner which just redirects output to xUnit's TestOutputHelper.

### 2.16.2 - 30 August 2021

* Support generation and shrinking of System.Collections.Immutable types via reflection.

### 2.16.1 - 28 August 2021

* Include inherited methods when registering Arbitrary instances. (by Bennie Copeland)

### 2.16.0 - 13 August 2021

* Support generating C# record types.

### 2.15.3 - 8 May 2021

* Allow shrinking private unions. (by Sander van Dijk)

* Limit total number of shrink attempts to 5000, to avoid infinite loops in general, and in particular if an argument is mutated in the test.

### 2.15.2 - 15 Arpil 2021

* Enabled FsCheck.Xunit's `PropertiesAttribute` to work at assembly level. (by Laurence King)

* Added `UnicodeString` and `UnicodeChar` generators.

### 2.15.1 - 27 February 2021

* Fixed a bug in FsCheck.Xunit: using ITestOutputHelper did not show output in `Property`-attributed tests.

### 2.15.0 - 23 February 2021

* Updated NUnit to 3.13.1, and set that as the lowest allowable version in FsCheck.NUnit package. Fixed resulting incompatibility.

* Since NUnit 3.13 no longer supports netstandard 1.6, removed netstandard 1.6 support from FsCheck.NUnit as well.

### 2.14.6 - 21 February 2021

* Restrict NUnit version range to versions < 3.13.1, as that introduced an incompatibility with FsCheck.NUnit.

### 2.14.5 - 18 February 2021

* Workaround for suspected F# compiler bug: prevented compiler from inlining `Prop.discard`. (by Okke Hendriks)

### 2.14.4 - 31 January 2021

* Fix NullRef in `isCSharpDtoType` when a property with only a setter was encountered.

### 2.14.3 - 9 June 2020

* Fix FsCheck.Xunit so it properly calls Dispose if the test class implements IDisposable. (by Laurence King)

### 2.14.2 - 11 March 2020

* Assembly metadata fix. This caused version 2.14.1 to be unusable in some cases; it was unlisted.

### 2.14.1 - 1 March 2020

* Add support for struct type generation and shrinking. (by Kody Musick)
* Support setting PropertyAttribute.Replay  to null. (by Eirik Tsarpalis)

### 2.14.0 - 17 April 2019

* Add shrinker for data transfer objects. (by Carlo Zancanaro)

* Add shrinker for Interval type. (by Nazar Vinnichuk)

* Add shrinker for flags enums.  (by Nazar Vinnichuk)

* Add new Arbitrary instances for Numerics.Complex and ConsoleKeyInfo. (by Nazar Vinnichuk)

* Fix float, int32 and int64 generators so their distribution is uniform. This also improves downstream generators. (by Nazar Vinnichuk)

* Fix decimal generator so size is respected. (by Nazar Vinnichuk)

* Add Gen.scale and ScaleSize extension method which can remove some boilerplate. (by Nazar Vinnichuk)

### 2.13.0 - 11 November 2018

* Stop shipping Portable Class Libraries (PCL), which are essentially deprecated by Microsoft in favor of .NET Standard. FsCheck now targets .NET Standard 1.0 (which takes the place of 3 PCLs that were previously included, while it still run on all those platforms and more), .NET Standard 1.6 (which is the lowest version modern FSharp.Core versions support) and .NET Standard 2.0 (which is advised by NuGet so clients don't need to download as much stuff if they can target 2.0 or higher). FsCheck also still targets .NET Framework 4.5.2 and upwards as a convenience.

* FsCheck.Xunit and FsCheck.NUnit similarly target .NET Standard 1.6 and 2.0, as well as .NET Framework 4.5.2. They've never targeted PCL before.

* It's my hope and expectation that this makes FsCheck usable on the same range of platforms (and more) as before, while greatly simplifying development: there is now just one solution (instead of three), and only one FsCheck project (instead of five) and so on.

* Add NonWhiteSpaceString to default generated types. (by Stijn Moreels)

### 2.12.1 - 18 October 2018

* Fix FsCheck.Xunit's CheckExtensions so that the throwing methods actually throw on failure.

* Add new generator extensions for C#: Zip, Zip3, Or and OrNull. (by Stijn Moreels)

* Add Gen.collect and variations. (by Stijn Moreels)

* Fix to offset of DateTimeOffset generator. (by Evgeny Grebenyuk)

### 2.12.0 - 6 October 2018

* Update NUnit to 3.10.1.

* Add Arbitrary instance for XML encoded string. (by Stijn Moreels)

### 2.11.0 - 15 June 2018

* DateTime now also generates kind and milliseconds (by Matthew Peacock)

* Removed EditorBrowsable.Never from all but the obsoleted methods. F# intellisense now also honors these attributes, and so they
were hiding a bunch of methods unnecessarily. The upshot is that C# users will see more of the API that is F# specific.

### 2.10.10 - 15 May 2018

* Gen.subListOf can now generate the entire given list, where previously it would always generate a strict sublist. (by Michał Niegrzybowski)

### 2.10.9 - 10 April 2018

* FsCheck.NUnit: Bugs fixed and added support for Ignore and Category attributes. (by Eirik Tsarpalis)

### 2.10.8 - 3 March 2018

* Add some C# extension members to combine boolean properties. (by Stijn Moreels)

### 2.10.7 - 29 January 2018

* Fix FsCheck.NUnit issue - failure output was not shown. (by adj123)

* Add C# Implies extension method. (by adj123)

### 2.10.6 - 30 December 2017

* NuGet packaging: relax FSharp.Core constraint to 4.0 and higher for .NET Desktop and PCL targets.

### 2.10.5 - 28 December 2017

* NuGet Packaging updates. (by Enrico Sada)

* Small performance improvements in Gen.apply and mapN.

### 2.10.4 - 8 November 2017

* Change NuGet dependency of FsCheck.Xunit and FsCheck.NUnit on FsCheck to strict equality constraint.

* Add specific dependency group to nuspec for net45 so that NuGet does not download a bunch of packages needlessly.

### 2.10.3 - 26 September 2017

* Don't escape newline and tab characters.

### 2.10.2 - 25 September 2017

* Fix stack overflow in Gen.piles, listOf and arrayOf when generating long collections.

### 2.10.1 - 23 September 2017

* Make Gen.variant thread safe.

* Allow registration of Arbitrary instances that are defined as properties, in addition to methods.

* Show control characters, which are unprintable, as escaped ascii codes in the output.

### 2.10.0 - 7 September 2017

* Update FsCheck.NUnit to work with NUnit 3.8.1. FsCheck.NUnit is not backwards compatible with earlier NUnit versions because NUnit changed the name of a few methods in their extension API.
* Gen.listOf and Gen.arrayOf now divide the size among the element generators, to avoid exponential explosion of size when high-dimensional types like `int list list list` are generated. This is the approach advocated in <https://gupea.ub.gu.se/handle/2077/22087>. The effect on list and array generators is as follows, quoting from that text: This adjustment of the list generator impacts the distribution in several ways. Most apparently it inverts the correlation between the length of the list and the size of the elements, long lists will tend to have small elements instead of large. Short lists with small elements will be generated whenever n is low. Short lists with large elements will occur when n is large and a small k is chosen. If a large k is chosen instead, long list with small elements will be generated. The only way to generate long lists with large elements is if n is very large, which is natural given that the purpose of the modified algorithm is to reduce the size of test data.
* Add Gen.piles generator, which generates a list of random elements that add up to a given sum.

### 2.9.2 - 28 August 2017

* Fix a bug in recursive union type generation that could lead to stack overflow in some cases.

### 2.9.1 - 27 August 2017

* Add NegativeInt type and Arbitrary instance. (by Stijn Moreels)
* Update SourceLink to v2. (by Cameron Taggart)

### 2.9.0 - 18 May 2017

* Add a .NET Standard 1.6 build to FsCheck, FsCheck.NUnit and FsCheck.Xunit NuGet packages. (with Enrico Sada)
* Update to latest FSharp.Core 4.1. The last version to support 3.1 or higher is FsCheck 2.8.2.

### 2.8.2 - 6 May 2017

* Add Gen.optionOf. (by Mark Seemann)

### 2.8.1 - 1 May 2017

* Added support to generate POCOs (reflectively) with a default ctor and settable properties. (by Giacomo Citi)

### 2.8.0 - 15 March 2017

* Removed support for .NET 4.5 and 4.5.1 frameworks as they are no longer supported by Microsoft.
* Updated to xUnit 2.2.
* Updated to NUnit 3.6.1.

### 2.7.2 - 5 March 2017

* FsCheck.Xunit now shows currently executing test in UI test runner. (by Lukas Rieger)

### 2.7.1 - 13 February 2017

* Improve reflection based generator for union cases - now also detect if a union case is recursive via any number of other types.
* Improve shrinking - when many possible values are tried, the shrinking was not tail recursive which could lead to stack overflow in rare situations. (by Lukas Rieger)
* Improvements to Experimental.StateMachine: some stats about generated operations are now printed and shrinking of operations can be customized (by Silvio Marcovic). Also the Setup shrinking phase now works correctly.

### 2.7.0 - 15 January 2017

* Switch FsCheck.NUnit to use NUnit 3. This solves a lot of issues with NUnit 2's wonky plugin model.
  FsCheck.NUnit.Addin is no longer needed, for example, and neither is the install.ps1 in the FsCheck.NUnit package. Also,
  FsCheck.NUnit only needs a dependency on NUnit now, which should solve a bunch of install issues on its own.
  (like xUnit 2 support, another heroic contribution by Jon Hamm, and brilliantly pushed over the finish line by Mark Lambert.)
* NUnit versions < 3.0 are no longer supported. The latest FsCheck version that supports NUnit 2.6.4 is FsCheck.NUnit 2.6.3.

### 2.6.3 - 13 January 2017

* Add Gen.zip, Gen.unzip, Gen.zip3 and Gen.unzip3. (by Mark Seemann)
* Add Cast() to enable type annotations in LINQ expressions. (by Mauricio Scheffer)

### 2.6.2 - 7 October 2016

* Add VB.NET support back. (by Paulmichael Blasucci)
* Add HostName generator. (by Mark Seemann)
* Add Check extenions to output FsCheck output to xUnit's output helper. (by Johannes Egger)

### 2.6.1 - 25 Septemer 2016

* Also generate and shrink IPV6 addresses in IPAddress generator. (by Mark Seemann)
* Add generators for Flags enums with non-int elements - previously only int elements were supported. (by Mark Seemann)

### 2.6.0 - 3 August 2016

* In FsCheck.Xunit, allow configuration on module level of all settings using PropertiesAttribute. Deprecated ArbitraryAttribute. (by Tomasz Heimowski)
* Add ToString for number wrappers so that output shows the underlying number (by Mauricio Scheffer)
* Faster array generation and shrinking (by Jack Pappas)
* Bug fix: Prop.classify did not take the last test into account. (by Silvio Marcovic)
* Bug fix: or property combinator failed overall test if an exception was thrown.

### 2.5.0 - 7 June 2016

* Add Gen.growingElements.
* Add default generator and shrinker for IPAddress and MailAddress.
* Gen.eval now uses the given size directly to generate an element of that size, instead of randomly choosing a size up to the given size. This means that on average test case sizes are bigger.
* Gen.frequency now throws a clearer exeption if no element can be generated.
* Renamed Gen.suchThat and Gen.suchThatOption to Gen.where or Gen.filter and Gen.tryWhere or Gen.tryFilter. (The functions/methods with the old names are still there, they are just marked Obsolete and will be removed in a future version.)
* Renamed DontSize to DoNotSize, DontShrink to DoNotShrink, DontSizeInt16 to DoNotSizeInt16 etc. (The types with the old names are still there, they are just marked Obsolete and will be removed in a future version.)
* StateMachine: operation shrinker shrinks operations too, not just the list of operations.

### 2.4.0 - 20 April 2016

* Fix bug in function generator - previously generated functions were constant functions.
* Add ThrowingFunction and Arb.Default.ThrowingFunction which generates a pure function, but that also throws exceptions.
* Add Gen.shuffle which generates random permutations of a given input sequence.
* Improvements and new features for FsCheck.Experimental.StateMachine: allow tracking of results of operations through the OperationResult type. The implicit
  dependencies encoded by one operation using the result of another, are taken into account by the shrinker.

### 2.3.0 - 11 April 2016

* Command shrinker now takes preconditions into account. Behavior could be slightly different, overall should be a clear improvement.
* Command generator now stops after trying a number of times when it can't find a command that satisfies the preconditions. Previously, it would loop forever.
* Exposed Command.generate and Command.shrink. Obsoleted Command.generateCommands in favour of Command.generate.
* Added Gen.where as a synonym for Gen.suchThat.
* Expanded StateMachine with stop command and max number of commands, better shrinking that also removes loops, and lots of bugfixes.

### 2.2.5 - 28 March 2016

* Fix timing of xUnit runs.
* Add static factory methods to Configuration like Config.
* Add function and method to Command module to allow users to specify generator and shrinker explicitly.
* Improvements to Experimental.StateMachine: better shrinking, expose some more API.

### 2.2.4 - 17 December 2015

* Fix output of FsCheck.Xunit - was not correctly reported in all runners (by Matt Ellis)
* Allow shrinking null for C# compatibility (by William Pleasant-Ryan)
* Fix default generators to not rely on static dictionary - this caused some weird side-effects and behavior when overriding default generators.

### 2.2.3 - 26 November 2015

* Now also reflectively generate union types with private constructors.
* Fix bug in xunit integration: Verbose didn't show any output.

### 2.2.2 - 14 November 2015

* Major performance improvements in generators, particularly reflection-based generators. (with M�rten R�nge)

### 2.2.1 - 5 November 2015

* Bug fix: xUnit runner did not update failed count correctly in some cases.
* Fix overly constrained FSharp.Core dependency in FsCheck NuGet package.

### 2.2.0 - 1 November 2015

* Add QuietOnSuccess option to vanilla runner.
* Add bigint generator.
* Deprecate some infrequently used and incorrectly named methods.
* Lots of internal and mostly syntactic code cleanup.
* Switch to paket for creating NuGet packages.

### 2.1.0 - 30 September 2015

* Switch to xunit 2.1. Note: Needs -noshadow argument on Mono due to Mono bug. A fix for Mono
  is submitted and is expected in Mono 4.3. (heroic achievement by Jon Hamm - thanks to all who tested early versions and found bugs.)
* xunit versions < 2.1 are no longer supported. The latest FsCheck version that supports xunit 1.9.2 is currently FsCheck.Xunit 2.0.7.
* Made Arbitrary overrides (e.g. using Arb.register, passing arbitrary to Config, or setting them
  in PropertyAttribute) thread-local. This is mainly to support xunit2: it runs tests in the same class
  in parallel by default. Not making the overrides thread local causes overrides from one test to mistakenly apply
  to others.
* FsCheck.Runner.init is Obsolete, it should not longer be necessary to call it explicitly. (though
  I am cautious here - please report any issues.)
* Fix for AppVeryor NuGet feed dependency problem.

### 2.0.7 - 20 August 2015

* Add assembly level ExtensionAttribute so VB.NET sees extension methods too. (by Paulmichael Blasucci)

### 2.0.6 - 14 August 2015

* Undo removal of GenBuilder.delay - this is needed otherwise while and for don't work correctly.
* Optimize GenBuilder.While.
* Add Gen.(>>=), monadic bind operator.

### 2.0.5 - 31 July 2015

* Make Gen operators <*> and <!> actual operators on the Gen type, instead of functions to avoid name clashes. (by Paul Young)
* Add FsCheck.Experimental.StateMachine for more advanced state machine-based checking, i.e. for mutable systems.
* Fix GenBuilder.delay so that generators inside gen computation expression are not needlessly re-created.
* Add Fun active pattern to make using shrinkable and printable functions easier.

### 2.0.4 - 20 July 2015

* Fix FsCheck.NUnit NuGet package.

### 2.0.3 - 10 July 2015

* Add support for Portable Class Library profiles 7,78 and 259.

### 2.0.2 - 7 July 2015

* Update NuGet package with new project home.
* Add missing frequency overload.
* Added some more C# examples to documentation.
* Internal changes related to use of reflection API to eventually support PCL.
* Fix bug in Commands.
* Improved output - now shows argument before and after shrinking.

### 2.0.1 - 23 May 2015

* Because apparently there there is some problem with FsCheck.Xunit and FsCheck.NUnit's NuGet dependencies to unstable versions.

### 2.0.1-rc1 - 23 May 2015

* Improve asssert failure reporting in FsCheck.Xunit (by Mark Seemann)
* Add discard method (by Mauricio Scheffer)
* Fix capitalization of Fscheck.NUnit package

### 2.0.0-alpha - 26 April 2015

* Rename FsCheckTestCaseBuider to Builder. (potentially breaking change)
* Add RequireQualifiedAccess to TestResult and Outcome. (breaking change for IRunner implementations and test framework integrations)
* Remove Fluent API and SpecBuilder in favor of overloads in Prop and extension methods on Property (breaking change)
* Add extension methods to allow working with Arbitrary from C#/VB.
* Rename Any to Gen. Rename pretty much all methods on Any to match the existing methods they wrap on Gen module. (breaking change)
* Add missing Gen and Arbitrary extension methods and C#/VB wrappers.
* Make Property a real type instead of a type alias.
* Add discard (by Mauricio Scheffer)
* Various improvements and changes to Command interface for easier consumption, also from C#/VB (breaking change)
* Add explicit dependency on FSharp.Core Nuget package to hopefully reduce version confusion and make things easier for C#/VB users.

### 1.0.4 - 9 December 2014

* Add NonNull<T>.

### 1.0.3

* Add option to FsCheck.Xunit and FsCheck.Nunit to suppress output on success.

#### 1.0.2 - 27 October 2014

* Moved default Arb registration to Arb module. This should fix issues
  folks have been having with the default generators not being registered in time.

#### 1.0.1 - 18 October 2014

* Add XML docs, mostly in C# API and xUnit integration attributes.
* Document FsCheck.Xunit and FsCheck.NUnit properly.

#### 1.0.0 - 3 August 2014

* Start using FAKE to automate releases
* Release NUnit addin
* Start using semantic versioning, hence the jump to 1.0.0
* Start releasing all NuGet packages simultaneously and with the same version.
* Start using FSharp.Formatting and publish documentation to github.io
* Start building for mono on non-windows platforms.
* Start using CI (appveyor for windows, Travis CI for MacOS).
* Update to .NET4.5 to take advantage of ExceptionDispatchInfo.
* Fix: sbyte overflow bug in shrinker.
* Fix: CultureInfo generator and shrinker.

#### 0.9.4 - 24 May 2014

* Update to F# 3.1
* Hardened other generators against the new null-generating string generator.

#### 0.9.3 - 9 May 2014

* String generator now generates null
