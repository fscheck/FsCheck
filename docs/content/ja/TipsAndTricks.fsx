(*** hide ***)
#I @"../../../src/FsCheck/bin/Release"
#r @"FsCheck"
open FsCheck
open System

(**
# ヒントとコツ

## 関数の性質

FsCheck はランダムな関数値を生成できるので、関数の性質を検査できます。
例えば、次のように関数合成の結合性を検査できます: *)

(***define-output:associativity***)
let associativity (x:int) (f:int->float,g:float->char,h:char->int) = ((f >> g) >> h) x = (f >> (g >> h)) x
Check.Quick associativity

(***include-output:associativity***)

(**
Tree -> _任意の型_ を生成できます。反例が見つかった場合は、関数値が <func> として表示されます。

しかしながら、 FsCheck は Function 型を使用することで、より詳細に生成された関数を表示することが出来ます。
もしそれを使えば、 FsCheck は関数をシュリンクすることさえ可能です。例は以下の通りです: *)

(***define-output:mapRec***)
let mapRec (F (_,f)) (l:list<int>) =
  not l.IsEmpty ==>
      lazy (List.map f l = ((*f <|*) List.head l) :: List.map f (List.tail l))
Check.Quick mapRec

(***include-output:mapRec***)

(**
`Function<'a,'b>` 型は呼ばれていたすべての引数、および生成した結果の写像を記録します。
性質では、例のようにパターンマッチによって実際の関数を抽出することが出来ます。
Function は関数を出力し、また関数をシュリンクするために使用されます。

## カスタムジェネレータを使うため forAll の代わりにパターンマッチを使用する

既存の型の普通の範囲の値の部分集合、例えばすべての偶数といったものを生成するジェネレータを定義するために、
1ケースだけの共用体を定義して新しい型用のジェネレータを登録すれば、性質をより読みやすくするでしょう:
 *)

(***define-output:EvenInt***)
type EvenInt = EvenInt of int with
  static member op_Explicit(EvenInt i) = i

type ArbitraryModifiers =
    static member EvenInt() = 
        Arb.from<int> 
        |> Arb.filter (fun i -> i % 2 = 0) 
        |> Arb.convert EvenInt int
        
Arb.register<ArbitraryModifiers>()

let ``generated even ints should be even`` (EvenInt i) = i % 2 = 0
Check.Quick ``generated even ints should be even``

(***include-output:EvenInt***)

(**
同様に、今やカスタムシュリンク関数を定義することも簡単です。

FsCheck は `NonNegativeInt` 、 `PositiveInt` 、 `StringWithoutNullChars` などでもこのパターンを使っています。
`Arb.Default` 型にあるデフォルトの Arbitrary インスタンスを参照してください。

また、この手のジェネレータにとって、 `Arb.filter` 、 `Arb.convert` そして `Arb.mapFilter` 関数は役に立つでしょう。

## 等式の左辺と右辺を出力する等価比較

性質は一般に等価性をチェックします。テストケースが失敗した場合、FsCheck は反例を出力しますが、
特に最初に生成された引数でいくつかの複雑な計算を行う場合は、比較の左辺と右辺も出力すると便利なことがあります。
これを簡単にするために、独自のラベル表示等価コンビネータを定義できます: *)

(***define-output:testCompare***)
let (.=.) left right = left = right |@ sprintf "%A = %A" left right

let testCompare (i:int) (j:int) = 2*i+1  .=. 2*j-1
Check.Quick testCompare

(***include-output:testCompare***)

(**
もちろん、あなたがよく使用する任意の演算子や関数に対してこれを行うことが出来ます。

## FsCheck のテストを実行するためのいくつかの方法

* あなたのプロジェクトにある fsx ファイルに性質やジェネレータを追加する。実行するのは簡単で、ctrl-a を押してから alt-enter を押すだけで、
  結果が F# Interactive に表示されます。ソリューションに組み込まれている dll を参照するときは注意してください。 F# Interactive は
  セッションの残りの間はずっとそれらをロックし、セッションを終了するまでビルドすることが出来ません。1つの解決策は、dll の代わりにソースファイルを
  (ソリューションに)含めることですが、それは処理を遅くします。小規模なプロジェクトに有用です。デバッグするのは難しいですが。
* 別のコンソールアプリケーションを作成する。デバッグは簡単ですし、アセンブリに迷惑なロックを行いません。テストのために FsCheck のみを使用し、
  性質が複数のアセンブリをまたぐような場合は、最良の選択肢です。
* 別のユニットテストフレームワークを使用する。 FsCheck とユニットテストの手法が混在し(いくつかのものはユニットテストを使ってチェックした方が簡単ですし、
  逆もまた然りです)、グラフィカルランナーを好む場合に便利です。あなたが使用しているユニットテストフレームワーク次第では、無料で Visual Studio と
  上手く統合できるでしょう。このシナリオで FsCheck をカスタマイズする方法は、上記を参照してください。
 *)
