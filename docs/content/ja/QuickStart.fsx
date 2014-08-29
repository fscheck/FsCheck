(*** hide ***)
#I "../../../src/FsCheck/bin/Release"
#r "../../../src/FsCheck.Xunit/bin/Release/FsCheck.Xunit.dll"
#r "../../../packages/xunit.1.9.2/lib/net20/xunit.dll"

(**
# クイックスタート

FsCheck がどのように動作するかを理解するための最も早い方法はいくつか *プロパティ(性質)* ― FsCheck のパラメタライズドテスト、いわゆる生成的テストのための
専門用語です ―を書き、それらを組み込みのテストランナーを使って実行することです。後ほど、それらがどのように NUnit や xUnit.NET、MsTest のような既存の
テストフレームワークと統合されるかを述べます。

はじめに FsCheck をインストールし、fsx ファイルを開き、次のように始めます: *)

#r "FsCheck"

open FsCheck

(**
## 簡単な例

性質 ― パラメータ付きのテスト ― の簡単な例は bool 値を返す普通の F# の関数として書かれます: *)

let revRevIsOrig (xs:list<int>) = List.rev(List.rev xs) = xs
(** この性質は、リストを逆順にして逆順にしたものは元のリスト自身になる、ということを主張します。
この性質を確かめるために、F# インタラクティブ上にこの定義をロードして、起動してみましょう *)

(*** define-output: revRevIsOrig ***)
Check.Quick revRevIsOrig

(*** include-output: revRevIsOrig ***)

(** 性質が失敗すると、FsCheck は反例を表示します。例えば、もし *)

let revIsOrig (xs:list<int>) = List.rev xs = xs

(** を定義すると、このような結果を確認できるでしょう: *)

(*** define-output: revIsOrig ***)
Check.Quick revIsOrig

(*** include-output: revIsOrig ***)

(** FsCheck は反例の *シュリンク* (訳註: 推定による絞り込みのこと)も行うので、テストケースを失敗させる最小の反例を見つけようとします。
反例は確かに最小、つまりリストは少なくともテストを失敗するための２つの異なる要素を持っています。FsCheck はより小さな反例を
(何らかの方法で)何回見つけてシュリンクを進めたかを表示します。

どのように性質を書くかについてもっと知りたいなら、 [性質](Properties.html) を参照してください。

## もしテストがループしたりエラーに出くわしたりしたら何をする？

性質が有効ではありませんが、Check.Quick が反例を表示しないという場合があります。このような場合のために、別のテスト関数があります。
テストを実行する前にそれぞれのテストケースを表示する
<pre>Check.Verbose &lt;property_name&gt;</pre>
を使ってもう一度テストしてみましょう。すなわち、最後に表示されたテストケースがループしているかエラーが発生しているものだということです。

FsCheck のテストを実行する方法についてもっと知りたいなら、 [テストの実行](RunningTests.html) を参照してください。

## 警告

上記の性質(逆順の逆順のリストは元のリスト自身)は常に正しいとは限りません。 `infinity` (無限大)や `nan` (非数(not a number))を含んだ
浮動小数点数のリストを考えてみましょう。 `nan <> nan` なので、もし単純に要素同士の比較を用いるなら `[nan, nan]` の逆順の逆順は
実際 `[nan, nan]` と等しくありません。FsCheck はこの手の仕様の問題を見つけ出すコツを備えています。しかし、この振る舞いはめったに
思ったとおりにならないので、型多相性(今のところ、unit, bool, char および string 値)を残しているなら FsCheck は「上手く」
比較できる値だけを生成します。このエラーを見るために、FsCheck に浮動小数点数のリストを生成させてみましょう:*)

let revRevIsOrigFloat (xs:list<float>) = List.rev(List.rev xs) = xs

(***define-output:revFloat***)
Check.Quick revRevIsOrigFloat

(***include-output:revFloat***)

(**
## FsCheck を他のテスティングフレームワークで使う

一度最初の FsCheck の探検が終われば、ユニットテストを増やすため、または単に性質をもっと簡単に実行するために、既存のユニットテストフレームワークで
使いたくなるでしょう。

### どんなユニットテストフレームワークでもサクッと統合

例として xUnit.NET を使ってみますが、同じ戦略をどんなテストフレームワークにも使うことが出来ます。どうやって上記のユニットテストを書くか
こちらに記載してあるので、xUnit.NET から実行することが出来ます:*)

open global.Xunit

[<Fact>]
let ``Reverse of reverse of a list is the original list``() =
  let revRevIsOrig (xs:list<int>) = List.rev(List.rev xs) = xs
  Check.QuickThrowOnFailure revRevIsOrig
  
(** xUnit にとって、このテストは普通のテストのように見えますし、もしテストが失敗したら必要な情報と一緒に例外が送出されることを QuickThrowOnFailure が
保証するので、xUnit はテストが失敗したことを検知します。テストの出力は上記と同様です。

### プラグインを使った xUnit.NET で FsCheck を使う

xUnit.NET は FsCheck プラグインの「恩恵に浴しています」。これを使うために、FsCheck.Xunit Nuget パッケージをインストールします。上記テストは
いまや次のようにより簡潔に書くことが出来ます:*)

open FsCheck.Xunit

[<Property>]
let ``Reverse of reverse of a list is the original list ``(xs:list<int>) =
  List.rev(List.rev xs) = xs
  
(** xUnit はこのテストが今やいつものテストに似ていることを示しており、これを直接実行することが出来ます。

この統合を使う方法についてもっと知りたいなら、 [テストの実行](RunningTests.html) を参照してください。 *)
