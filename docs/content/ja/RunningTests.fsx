(*** hide ***)
#I "../../../src/FsCheck/bin/Release"
#r @"../../../packages/xunit.1.9.2/lib/net20/xunit.dll"
#r "FsCheck"

open FsCheck
open System

(**
# テストの実行

このセクションでは FsCheck のテストを実行する様々な方法を説明します:

* FsCheck は F# 対話環境やコマンドラインプログラム、そしてどんなテストフレームワークからでも簡単に実行できる組み込みのテストランナーを持っています。
テスト結果を標準出力に書き、テストが失敗した場合に FsCheck ランナーが例外を投げるように設定することができます。

* FsCheck.Xunit は性質やジェネレータを簡潔な方法で特定できるよう、 xUnit.NET と強く統合されたものです。
この方法で書かれたテストは xUnit.NET のテストのように見えます。

* FsCheck では結果や実行するテスト自体を呼び出す IRunner 実装を登録することができます。
これにより、特定のテストフレームワークに対してより強く統合させることができたり、結果の出力をよりコントロールできるようにしてくれます。

## 組み込みテストランナーを使う

### fsx ファイルから、またはコマンドラインランナーから

このガイドの前のセクションをお読みになられた方は、この方法についてそろそろご存知のはずです。関数のように性質をシンプルに書くことができ、
`Check` 型のメソッドの1つを呼び出します。

`Check` のメソッドについて、基本的に2つのバージョンがあります: `Check.One` と `Check.All` です。

`Check.One` は1つの性質に対してテストを実行します。引数として `Config` 型のインスタンスを取りますが、これは FsCheck テストランナーの挙動に関する様々な設定、
とりわけテストデータのサイズや使用されるランダムシードの設定を行うことができるようにします(ですので、実行を再現することができます)。 `Check` 上のほとんどのメソッドは
特定の設定でテストを実行するための略記です。例えば、 `Check.Quick` は `Check.One(Config.Quick, <property>)` と同等です。

`Config.Verbose` と `Check.Verbose` に注意しておきましょう。これらはテストごとの引数を印字するので、例えばある入力でテストが無限ループした場合に便利です。

`Check.QuickThrowOnFailure` と `Check.VerboseThrowOnFailure` にも注意しておきましょう。これらは別のテストフレームワークのユニットテストに使われることを
意図しています。FsCheck ランナーを直接呼び出し、失敗した場合には(例外を)送出するように設定します。私が知る全てのフレームワークで、これはテストが失敗したということを示しているので、
簡単に FsCheck のテストと既にお持ちのユニットテストを合わせることができます。

### Check.All でグループ化された性質をテストする

大抵、テストするための性質を複数書くでしょう。FsCheck はクラスの静的メンバーとして性質をグループ化できるようにします: *)
type ListProperties =
  static member ``reverse of reverse is original`` (xs:list<int>) = List.rev(List.rev xs) = xs
  static member ``reverse is original`` (xs:list<int>) = List.rev xs = xs
(**これらは使うときに1度だけチェックされます: *)

(***define-output:ListProperties***)
Check.QuickAll<ListProperties>()

(**FsCheck はそれぞれのテストの名前も印字します: *)

(***include-output:ListProperties***)

(**モジュールの全てのトップレベル関数がモジュール名とともにクラスの静的メンバーとしてコンパイルされるので、あるモジュールの全てのトップレベル関数を
テストするのに Check.QuickAll を使うこともできます。
しかしながら、モジュールの型は F# から直接アクセスできないので、以下のトリックを使うことができます: *)

(***define-output:ListProperties2***)
Check.QuickAll typeof<ListProperties>.DeclaringType

(**
`Check.Verbose` の対になるものについても知っておきましょう: `Check.VerboseAll` です。

### モジュールだけを使ったテストの実行

Arbitrary インスタンスはクラスの静的メンバーとして与えられ、性質はクラスの静的メンバーとしてグループ化することができます。
トップレベルの let 関数はそれらを囲むモジュール(クラスとしてコンパイルされます)の静的メンバーとしてコンパイルされるので、
単に性質やジェネレータをトップレベルの let 束縛関数として定義でき、次のトリックを使って全てのジェネレータと全ての性質をすぐに登録することができます: *)

(***define-output:Marker***)
let myprop (i:int) = i >= 0
let mygen = Arb.Default.Int32() |> Arb.mapFilter (fun i -> Math.Abs i) (fun i -> i >= 0)
let helper = "a string"
let private helper' = true

type Marker = class end
Arb.registerByType (typeof<Marker>.DeclaringType)
Check.QuickAll (typeof<Marker>.DeclaringType)

(***include-output:Marker***)

(**
Maker 型はモジュール内に定義されている型なら何でもよく、モジュールの Type が取得できるようにします。F# は直接モジュールの型を
取得する方法を提供していません。

FsCheck は戻り値の方に基づいて関数の意図を決定します:

* 性質: unit、bool、Property、または任意の引数からこれらの型への関数、またはこれらの型の Lazy 値を返す public な関数
* Arbitrary インスタンス: Arbitrary<_> を返す

他のすべての関数は丁重に無視されます。もし FsCheck が何かを行うという型を返す関数をトップレベルに持っていて、しかしそれらをチェックしたり
登録したりしたくない場合、単にそれらを private にするだけです。FsCheck はこれらの関数を無視します。

## FsCheck.Xunit を使う

TODO

## IRunner を実装する

### 例1: FsCheck と他のユニットテストフレームワークを統合する

`Check.One` や `Check.All` に渡すことができる `Config` 型は、引数として `IRunner` を取ります。
このインターフェイスは次のメソッドを持っています:

* `OnStartFixture` は FsCheck がある型のすべてのメソッドをテストする時に、任意のテストを開始する前に呼び出されます。
* `OnArguments` はテスト番号や引数、あらゆる関数の実装を渡して、全てのテスト後に呼び出されます。
* `OnShrink` は全ての成功したシュリンクに対して呼び出されます。
* `OnFinished` はテストの名前と全体的なテスト実行の結果を伴って呼び出されます。これは以下の例で特定のユニットテストフレームワーク
― FsCheck が簡単に統合できる ― からAssert 文を呼び出すために使われます。テストを setup や tear down し、素敵なグラフィカルランナーを持つなど
別のユニットテストフレームワークの能力を行使することができます *)

open Xunit

let xUnitRunner =
  { new IRunner with
      member x.OnStartFixture t = ()
      member x.OnArguments (ntest,args, every) = ()
      member x.OnShrink(args, everyShrink) = ()
      member x.OnFinished(name,testResult) = 
          match testResult with 
          | TestResult.True _ -> Assert.True(true)
          | _ -> Assert.True(false, Runner.onFinishedToString name testResult) 
  }
   
let withxUnitConfig = { Config.Default with Runner = xUnitRunner }

(**
### 例2: 生成された引数の印字をカスタマイズする

デフォルトでは、FsCheck は`sprintf "%A"` 、すなわち構造化フォーマットを使用して生成された引数を印字します。これは通常あなたの期待通りのこと、つまり、
プリミティブ型に対しては値を、オブジェクトに対しては ToString のオーバーライドを表示します。もしそうでなければ(動機のあるケースは COM オブジェクトをテストすることです
― オーバーライドされた ToString は選択肢になく、構造化フォーマットは役に立つようなことは何もしません)、これを性質ごとに解決するために `label` コンビネータを
使うことができますが、より構造化された解決策は `IRunner` を実装することで達成できます。例は以下のとおりです: *)
    
let formatterRunner formatter =
  { new IRunner with
      member x.OnStartFixture t =
          printf "%s" (Runner.onStartFixtureToString t)
      member x.OnArguments (ntest,args, every) =
          printf "%s" (every ntest (args |> List.map formatter))
      member x.OnShrink(args, everyShrink) =
          printf "%s" (everyShrink (args |> List.map formatter))
      member x.OnFinished(name,testResult) = 
          let testResult' = match testResult with 
                              | TestResult.False (testData,origArgs,shrunkArgs,outCome,seed) -> 
                                  TestResult.False (testData,origArgs |> List.map formatter, shrunkArgs |> List.map formatter,outCome,seed)
                              | t -> t
          printf "%s" (Runner.onFinishedToString name testResult') 
  }
