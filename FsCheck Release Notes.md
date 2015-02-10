### 2.0.0-alpha - To be released
  * Rename FsCheckTestCaseBuider to Builder. (potentially breaking change)
  * Add RequireQualifiedAccess to TestResult and Outcome. (breaking change for IRunner implementations and test framework integrations)
  * Rename UnbrowsableObject to Specification (breaking change)
  * In the Spec interface, if a generator but no shrinker is given, don't use the shrinker derived from the type (breaking change)
  * Add extension methods to allow working with Arbitrary from C#/VB. 
  * Add missing arities of Spec.For and add overloads that take Arbitrary.
  * Rename Any to Gen. Rename pretty much all methods on Any to match the existing methods they wrap on Gen module. (painful breaking change)
  * Add missing Gen and Arbitrary extension methods and C# wrappers.

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
