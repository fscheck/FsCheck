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
  * Gen.eval now uses the given size directly to generate an element of that size, instead of randomly choosing a size up to the given size. This means that on average
    test case sizes are bigger.
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
