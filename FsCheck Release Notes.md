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
