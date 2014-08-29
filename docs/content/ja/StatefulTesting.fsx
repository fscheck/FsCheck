(*** hide ***)
#I @"../../../src/FsCheck/bin/Release"
#r @"FsCheck"
open FsCheck
open System

(**
# モデルベースのテスト

たいていの場合オブジェクトは一連のメソッドによって内部状態がカプセル化されているわけですが、
FsCheck ではこのようなオブジェクトを対象にしたテストを実行することもできます。
FsCheck ではほんの少し手を加えるだけで、テスト対象となるクラスに対してモデルを基準とするような
仕様を定義できます。たとえば以下のような人為的なバグが含まれているクラスがあるとします: *)

type Counter() =
  let mutable n = 0
  member x.Inc() = n <- n + 1
  member x.Dec() = if n > 2 then n <- n - 2 else n <- n - 1
  member x.Get = n
  member x.Reset() = n <- 0
  override x.ToString() = n.ToString()

(**
このクラスをテストするためのモデルとしては、オブジェクトの内部状態を表すのに役に立ちそうな int 値1つがふさわしいでしょう。
それを踏まえると、以下のような仕様が作成できます:
 *)

open FsCheck.Commands

let spec =
  let inc = 
      { new ICommand<Counter,int>() with
          member x.RunActual c = c.Inc(); c
          member x.RunModel m = m + 1
          member x.Post (c,m) = m = c.Get |> Prop.ofTestable
          override x.ToString() = "inc"}
  let dec = 
      { new ICommand<Counter,int>() with
          member x.RunActual c = c.Dec(); c
          member x.RunModel m = m - 1
          member x.Post (c,m) = m = c.Get |> Prop.ofTestable
          override x.ToString() = "dec"}
  { new ISpecification<Counter,int> with
      member x.Initial() = (new Counter(),0)
      member x.GenCommand _ = Gen.elements [inc;dec] }

(**
仕様は `ISpecification<'typeUnderTest,'modelType>` を実装したオブジェクトです。仕様は、初期状態のオブジェクトと、
そのオブジェクトに対する初期状態のモデルを返さなくてはいけません。また ICommand オブジェクトのジェネレータも返す必要があります。

それぞれの ICommand オブジェクトでは、一般的にはテスト対象のオブジェクトに対する1つのメソッド呼び出しに対応するようにして、
コマンドを実行することでモデルとオブジェクトに対して起こることを定義します。また、コマンドを実行する前に満たすべき事前条件を
アサートします。すなわち、もしも前提条件が一致しないのであれば FsCheck はそのコマンドを実行しません。コマンドの実行後は
一致すべき事後条件もチェックされます。すなわち、事後条件が一致しない場合、FsCheck ではテストが失敗したものと判断されます。

なお反例が表示できるように ToString をオーバーライドしておくとよいでしょう。

仕様を以下のようにチェックできます: *)

(***define-output:spec***)
Check.Quick (asProperty spec)

(***include-output:spec***)

(**
FsCheck は「バグ」を発見しただけでなく、バグを発生させる最小のシーケンスも生成したことにも注目してください。 *)