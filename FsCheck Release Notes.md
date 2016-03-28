### 2.2.5 - To be released
  * Fix timing of xUnit runs.
  * Add static factory methods to Configuration common cases.
  * Add function and method to Command module to allow users to specify generator and shrinker explicitly.

### 2.2.4 - 17 December 2015
  * Fix output of FsCheck.Xunit - was not correctly reported in all runners (by Matt Ellis)
  * Allow shrinking null for C# compatibility (by William Pleasant-Ryan)
  * Fix default generators to not rely on static dictionary - this caused some weird side-effects and behavior when overriding default generators.

### 2.2.3 - 26 November 2015
 * Now also reflectively generate union types with private constructors.
 * Fix bug in xunit integration: Verbose didn't show any output.

### 2.2.2 - 14 November 2015
 * Major performance improvements in generators, particularly reflection-based generators. (with Mårten Rånge)

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
