(*** hide ***)
#I "../../../src/FsCheck/bin/Release"
#r "FsCheck"

open FsCheck
open System

(**
# 性質

性質は F# の関数定義として表現されます。性質はそのパラメータにわたって例外なく定量化されるので、 *)

let revRevIsOrig xs = List.rev(List.rev xs) = xs

(**
は等式が全てのリスト xs で成り立つことを意味します。

性質は決して単相型を持ってはいけません。
上記のような「多相的な」性質は、まるでジェネリックな引数が型オブジェクトであるかのようにテストされます。これは、
様々な単純型(bool, char, string, ...)が生成されることを意味します。
それは1つの生成されるリストに複数の型が含まれる場合さえあるかもしれないということで、例えば `{['r', "la", true]}`
は上記の性質をチェックするために使うことができるリストになりえます。
ですが生成された値は型をベースにしているので、推論により、あるいは明示的に、xs に異なる型を与えることで、
この振る舞いを簡単に変更することもできます。つまり、 *)

let revRevIsOrigInt (xs:list<int>) = List.rev(List.rev xs) = xs

(** は int のリストについてのみチェックされます。

FsCheck は様々な形式の性質をチェックすることができますーこれらの形式はテスト可能であると呼ばれ、
`'Testable` というジェネリック型によって API において示されます。 `'Testable` は
bool または unit を返す任意個のパラメータを取る関数となるでしょう。後者の場合、
もし(例外を)送出しなければテストはパスします。

## 条件付き性質

性質は `<condition> ==> <property>` という形式を取ることがあります。

例えば、 *)

(***hide***)
let rec ordered xs = 
  match xs with
  | [] -> true
  | [x] -> true
  | x::y::ys ->  (x <= y) && ordered (y::ys)
let rec insert x xs = 
  match xs with
  | [] -> [x]
  | c::cs -> if x <= c then x::xs else c::(insert x cs)

(***define-output:insertKeepsOrder***)
let insertKeepsOrder (x:int) xs = ordered xs ==> ordered (insert x xs)
Check.Quick insertKeepsOrder

(***include-output:insertKeepsOrder***)

(**
条件が成り立つときはいつでも `==>` の後の性質が成り立つのなら、このような性質は成り立ちます。

テストは条件を満たさないテストケースを切り捨てます。条件を満たすテストケースが100件見つかるまで、
あるいはテストケース数の限度に達する(条件が決して成り立たないループを避けるため)まで、テストケースの生成は続けられます。
この場合、 "Arguments exhausted after 97 tests." というようなメッセージは、条件を満たすテストケースが97件見つかり、
その97件に対して性質が成り立った、ということを示します。

この場合、生成された性質は int 型に制限されなければならなかったことに注意してください。なぜなら生成された値は
比較可能である必要がありましたが、これは型に反映されていません。ですから、明示的な制限がなければ、
FsCheck は異なる型(object の派生型)を含むリストを生成したかもしれず、そしてこれらは互いに比較可能ではありません。

## 遅延性質

F# はデフォルトでは正則評価ですが、上記の性質は必要以上に働いてしまいます。つまり、
左側の条件の結果がどうであれ、条件の右側にある性質を評価します。
上の例におけるパフォーマンス面の考察だけではありますが、性質の表現力を制限する可能性がありますー考えてみましょう: *)

(***define-output: eager***)
let tooEager a = a <> 0 ==> (1/a = 1/a)
Check.Quick tooEager

(***include-output: eager***)

(**
ここで、性質が正しくチェックされているかどうかを確かめるために、遅延評価が必要になります: *)

(***define-output: lazy***)
let moreLazy a = a <> 0 ==> (lazy (1/a = 1/a))
Check.Quick moreLazy

(***include-output: lazy***)

(**
## 定量化された性質

性質は `forAll <arbitrary> (fun <args> -> <property>)` という形式を取る場合があります。

例えば、 *)

(***define-output:insertWithArb***)
let orderedList = Arb.from<list<int>> |> Arb.mapFilter List.sort ordered
let insertWithArb x = Prop.forAll orderedList (fun xs -> ordered(insert x xs))
Check.Quick insertWithArb

(***include-output:insertWithArb***)

(**
forAll の第一引数は IArbitrary インスタンスです。このようなインスタンスは
テストデータのジェネレータとシュリンカ(詳しくは [テストデータ](TestData.html))をカプセル化します。
その型のデフォルトであるジェネレータを使う代わりに、お手製のジェネレータを与えることで、
テストデータの分布をコントロールすることができます。この例では、
整序済みリスト用の自作ジェネレータを与えることで、整序されていないテストケースをフィルタリングする
というよりも、むしろテストケースの総合的な限界に達することなく100のテストケースを生成できることを保証します。
ジェネレータを定義するためのコンビネータを [テストデータ](TestData.html) で説明します。

## 例外の予測

ある状況下で関数やメソッドが例外をスローするというテストをしたいと思うかもしれません。
これを実現するために `throws<'e :> exn, 'a> Lazy<'a>` を使いましょう。例えば: *)

(***define-output: expectDivideByZero***)
let expectDivideByZero() = Prop.throws<DivideByZeroException,_> (lazy (raise <| DivideByZeroException()))
Check.Quick expectDivideByZero

(***include-output: expectDivideByZero***)
  
(**
## 時間制限のある性質

性質は `within <timeout in ms> <Lazy<property>>` という形式を取ることがあります。

例えば、 *)

let timesOut (a:int) = 
    lazy
        if a>10 then
            do Threading.Thread.Sleep(3000)
            true
        else 
            true
    |> Prop.within 1000

(**
第一引数は与えられた遅延性質が実行して良い最大の時間です。もしそれよりも長く実行していると、
FsCheck はそのテストが失敗したと見做します。そうでなければ、遅延性質の結果は within の結果です。
within が、性質が実行されているスレッドをキャンセルしようとしても、
それはきっと上手くいかないでしょうし、そのスレッドはプロセスが終了するまで実際に実行し続けるであろうということに注意してください。

## テストケース分布の観測

テストケースの分布を意識しておくのは重要な事です。つまり、もしテストデータがよく分布していなかったら、
テスト結果から導き出される結論は正しくないかもしれません。特に、与えられた性質を満足するデータだけが使われるので、
`==>` 演算子はテストデータの分布を不適切に歪めます。

FsCheck はテストデータの分布を観測する手段を幾つか提供します。観測するためのコードは性質の宣言に含まれ、
性質が実際にテストされる度に測定され、収集した観測結果はテストが完了した時に集約されます。

## 自明なケースの計数

性質は `trivial <condition> <property>` という形式を取ることがあります。

例えば、 *)

(***define-output:insertTrivial***)
let insertTrivial (x:int) xs = 
  ordered xs ==> (ordered (insert x xs))
  |> Prop.trivial (List.length xs = 0)
Check.Quick insertTrivial

(**
この条件が真になるテストケースは自明であると分類され、全体における自明なテストケースの比率が報告されます: *)

(***include-output:insertTrivial***)

(**
### テストケースの分類

性質は `classify <condition> <string> <property>` という形式を取ることがあります。

例えば、 *)

(***define-output:insertClassify***)
let insertClassify (x:int) xs = 
  ordered xs ==> (ordered (insert x xs))
  |> Prop.classify (ordered (x::xs)) "at-head"
  |> Prop.classify (ordered (xs @ [x])) "at-tail"
Check.Quick insertClassify

(**
条件を満足するテストケースは与えられた分類に割り当てられ、分類の分布はテスト後に報告されます: *)

(***include-output:insertClassify***)

(**
1つのテストケースは複数の分類に当てはまる場合があることに注意してください。

### データの値の収集

性質は `collect <expression> <property>` という形式を取ることがあります。

例えば、 *)

(***define-output: insertCollect***)
let insertCollect (x:int) xs = 
  ordered xs ==> (ordered (insert x xs))
      |> Prop.collect (List.length xs)
Check.Quick insertCollect

(**
collect の引数はテストケースごとに評価され、値の分布が報告されます。
この引数の型は `sprintf "%A"` を用いて出力されます: *)

(***include-output: insertCollect***)

(**
### 観測の連結

ここで説明した観測は何らかの方法で連結されるかもしれません。テストケースそれぞれにおける
全ての観測は連結されており、これらの連結の分布は報告されます。例えば *)

(***define-output:insertCombined***)
let insertCombined (x:int) xs = 
    ordered xs ==> (ordered (insert x xs))
    |> Prop.classify (ordered (x::xs)) "at-head"
    |> Prop.classify (ordered (xs @ [x])) "at-tail"
    |> Prop.collect (List.length xs)
Check.Quick insertCombined

(***include-output:insertCombined***)

(**
## And, Or およびラベル

性質は

* `<property> .&. <property>` は両方成功した場合に成功し、性質のどちらか一方が失敗した場合に失敗し、両方とも棄却された場合に棄却されます。
* `<property> .|. <property>` は性質のどちらかが成功した場合に成功し、両方の性質が失敗した場合に失敗し、両方とも棄却された場合に棄却されます。

`.&.` コンビネータは、ジェネレータを共有する複雑な性質を記述するために最も一般的に使われます。
この場合、失敗時にどのサブプロパティ(従属性質)が原因で失敗したか正確に知るのが難しいことがあるかもしれません。
そのために従属性質にラベルを付けることができて、FsCheck が反例を見つけると、失敗した従属性質のラベルを表示します。
これはこのような形式になります: `<string> @| <property>` または `<property> |@ <string>`

例えば、 *)

(***define-output:complex***)
let complex (m: int) (n: int) =
  let res = n + m
  (res >= m)    |@ "result > #1" .&.
  (res >= n)    |@ "result > #2" .&.
  (res < m + n) |@ "result not sum"
Check.Quick complex

(***include-output:complex***)

(**
1つの性質に複数のラベルを適用することは一向に構いません。FsCheck は適用可能な全てのラベルを表示します。
これは途中の結果を表示するのに便利で、例えば: *)

(***define-output:multiply***)
let multiply (n: int, m: int) =
    let res = n*m
    sprintf "evidence = %i" res @| (
      "div1" @| (m <> 0 ==> lazy (res / m = n)),
      "div2" @| (n <> 0 ==> lazy (res / n = m)),
      "lt1"  @| (res > m),
      "lt2"  @| (res > n))
Check.Quick multiply

(***include-output:multiply***)

(**
上記の性質は従属性質をタプルにすることで連結していることに注意しましょう。これは長さ6のタプルまで上手くいきます:

* `(<property1>, <property2>, ..., <property6>)` は `<property1> .&. <property2> .&. ... .&. <property6>` を意味します
* `[property1; property2, ..., propertyN]` は `<property1> .&. <property2> .&. ... .&. <propertyN>` を意味します

リストとして記述した例: *)

let multiplyAsList (n: int, m: int) =
    let res = n*m
    sprintf "evidence = %i" res @| [
      "div1" @| (m <> 0 ==> lazy (res / m = n));
      "div2" @| (n <> 0 ==> lazy (res / n = m));
      "lt1"  @| (res > m);
      "lt2"  @| (res > n)]

(**
は同じ結果となります。 *)
