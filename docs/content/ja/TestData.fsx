(*** hide ***)
#I @"../../../src/FsCheck/bin/Release"
#r @"FsCheck"
open FsCheck
open System

(**
# テストデータ: ジェネレータ、シュリンカ、 Arbitrary インスタンス

テストデータはテストデータジェネレータによって作成されます。FsCheck では、
いくつかのよく使用される型については規定のジェネレータが定義されていますが、自分で定義することができ、
導入した新しい型については自身でジェネレータを定義する必要があります。

ジェネレータは `Gen<'a>` という形式の型を持ちます。これは型 a の値のためのジェネレータです。
自分のジェネレータをビルドするために、 `gen` と呼ばれるコンピュテーション式が FsCheck から提供され、
`Gen` モジュールにあるすべての関数が自由に利用できます。

シュリンカは `'a -> seq<'a>` という形式の型を持ちます。値を1つ受け取ると、
ある方法により受け取った値より小さな値からなるシーケンスを生成します。
FsCheck は、与えられた性質を満たさない値セットを見つけると、値のシュリンクを取得して、
それぞれを順番に試して性質が満たされないことを確認することにより、
元の(ランダムな)値より小さな値を作ろうとします。もし存在すれば、見つかった小さな値が新しい反例となり、
その値とともにシュリンク過程が継続します。

シュリンカに FsCheck からの特別なサポートはありません。言い換えれば、これは必要ないということです。
なぜならば、必要なものはすべて `seq` コンピュテーション式と `Seq` モジュールにあるからです。

最後に、 `Arbitrary<'a>` インスタンスは、性質で使われるようにこれら2つの型をまとめます。
FsCheck では Arbitrary インスタンスを、 `Type` から `Arbitrary` への辞書に登録することもできます。
この辞書は引数の型に基づき、その引数を持つ性質の任意のインスタンスを見つけるために使われます。

`Arb` モジュールには Arbitrary インスタンスのためのヘルパ関数があります。

## ジェネレータ

ジェネレータは関数 `choose` により構築され、一様分布を利用した区間から値のランダム抽出を行います。
例えばリストからの要素のランダム抽出を行いたい場合、 *)

let chooseFromList xs = 
  gen { let! i = Gen.choose (0, List.length xs-1) 
        return (List.nth xs i) }
  
(**
を使用します。

### 選択肢から選ぶ

ジェネレータは、リスト内のジェネレータから等確率で選ぶ `Gen.oneof <sequence of generators>`
のような形式を取るかもしれません。例えば、 *)

Gen.oneof [ gen { return true }; gen { return false } ]

(**
は確率 1/2 で true になるランダムな bool 値を生成します。

`frequency` を使用して、結果の分布を制御することができます。
`frequency` はリストからジェネレータをランダムに選びますが、それぞれの選択肢が選ばれる確率は
与えられた要素により重み付けされます。例えば、 *)

Gen.frequency [ (2, gen { return true }); (1, gen { return false })]

(**
は 2/3 の確率で true を生成します。

### テストデータサイズ

テストデータジェネレータは暗黙のサイズパラメータを持ちます。FsCheck は小さなテストケースを
作ることから始め、テストが進むにつれて徐々にサイズを増加させます。
テストデータジェネレータによってサイズパラメータの解釈方法が異なります。
あるものはそれを無視しますが、例えばリストジェネレータはそれを生成されるリストの長さの
上限として 解釈します。使用するかどうかは、テストデータジェネレータを制御したいかどうかによります。

サイズパラメータの値は `sized` を使用することで取得できます。 `sized g` は現在のサイズをパラメータとして
`g` を呼びます。例えば、0 から size までの自然数を生成するには、 *)

Gen.sized <| fun s -> Gen.choose (0,s)

(**
サイズ制御の目的は、エラーを見つけるのに充分大きく、しかもテストを素早く行うのに充分小さいテストケースを
確保するためにあります。既定のサイズ制御ではこれを達成できない場合があります。
例えば、1回のテストランの終わりまでに任意のリストは50要素まで作られますが、これはつまりリストのリストは2,500要素となり、
効率的なテストとしては大きすぎます。このようなケースでは、明示的にサイズパラメータを変更するのがよいでしょう。
そのようにするには `resize` を使用します。

`resize n g` はサイズパラメータ `n` とともに `g` を実行します。サイズパラメータは負になってはいけません。
例えば、ランダム行列を生成するために元のサイズの平方根をとるのは適切でしょう。 *)

let matrix gen = Gen.sized <| fun s -> Gen.resize (s|>float|>sqrt|>int) gen

(**
### 再帰的データ型の生成

再帰的データ型のためのジェネレータは、コンストラクタを選択するために `oneof` あるいは `frequency` を、
また各ケース用のジェネレータを作るために F# の標準的なコンピュテーション式の構文を使用することで、
容易に表現することができます。また、コンストラクタや関数を `Gen` 型に持ち上げる、6個までのアリティ用の
`map 関数もあります。例えば、木の型が *)

type Tree = Leaf of int | Branch of Tree * Tree

(**
で定義されているとすると、木のためのジェネレータは *)

let rec unsafeTree() = 
  Gen.oneof [ Gen.map Leaf Arb.generate<int> 
              Gen.map2 (fun x y -> Branch (x,y)) (unsafeTree()) (unsafeTree())]

(**
で定義できるでしょう。しかしながら、このような再帰ジェネレータはおそらく停止できずに StackOverflowException
とともに失敗するか、とても大きな結果を生じるでしょう。これを防ぐために、
再帰ジェネレータは常にサイズ制御機構を使用するべきです。例えば、 *)

let tree =
    let rec tree' s = 
        match s with
        | 0 -> Gen.map Leaf Arb.generate<int>
        | n when n>0 -> 
            let subtree = tree' (n/2)
            Gen.oneof [ Gen.map Leaf Arb.generate<int> 
                        Gen.map2 (fun x y -> Branch (x,y)) subtree subtree]
        | _ -> invalidArg "s" "Only positive arguments are allowed"
    Gen.sized tree'

(**
 以下に注意してください。

 - サイズが 0 であるときに結果が葉になることを強制することで停止を保証しています。
 - 再帰ごとにサイズを半分にします。これによりサイズは木におけるノード数の上限となります。解釈したいようにサイズを自由に解釈できます。
 - 2つの枝間で1つの部分木を共有しているという事実は、ケースごとに同じ木を生成するということを意味しません。


 ### 便利なジェネレータコンビネータ

 `g` を型 `t` に対するジェネレータとすると、

 - `two g` は t の 2 つ組を生成します。
 - `three g` は t の 3 つ組を生成します。
 - `four g` は t の 4 つ組を生成します。
 - xs をリストとすると、 `elements xs` は xs の任意の要素を生成します。
 - `listOfLength n g` はちょうど n 個の t のリストを生成します。
 - `listOf g` は長さがサイズパラメータによって決められる t の空でないリストを生成します。
 - `constant v` は値 v を生成します。
 - `suchThat p g` は述語 p を満足する t を生成します。述語を満たす確率が高いようにしてください。
 - `suchThatOption p g` は述語 p を満足するならば Some t を、見つからなければ None を生成します。(「一生懸命」やった後で)


 これらすべてのジェネレータコンビネータは Gen モジュールの関数です。

 ## 型に基づく既定のジェネレータとシュリンカ

 FsCheck には、よく使用される型(unit, bool, byte, int, float, char, string, DateTime, リスト, 一次元および二次元の配列、Set,
 Map, オブジェクト、以上の型から型への関数)については既定のテストデータジェネレータとシュリンカが定義されています。
 さらに、リフレクションを用いて、 FsCheck はあらゆるプリミティブ型に関して(FsCheck 内に、あるいはユーザーによって)定義されたレコード型、
 判別共用体、タプル、列挙型の既定の実装を得ることができます。

 これらは性質ごとに明示的に定義する必要はありません。FsCheck は、すべての性質の引数について、もしジェネレータやシュリンカを知っていたり、
 得られたりするならば、適切なジェネレータとシュリンカを指定したプロパティを提供することができます。
 たいていの場合、性質に基づいたこれらの型を見つけ出す仕事を型推論に任せることができます。しかしながら FsCheck に特定のジェネレータと
 シュリンカを強制したい場合は、適切な型注釈を与えることによって可能になります。

 FsCheck は特定の型に対するジェネレータとシュリンカを `Arbitrary` 型としてまとめます。 `Arbitrary<'a>` を継承したクラスの
 インスタンスを返すようなクラスの静的メンバを定義することで、独自型に対する Arbitrary インスタンスを FsCheck に与えることができます。 *)

type MyGenerators =
  static member Tree() =
      {new Arbitrary<Tree>() with
          override x.Generator = tree
          override x.Shrinker t = Seq.empty }

(**
`'a` を Arbitrary インスタンスを定義したい特定の型に置き換えてください。 `Generator` メソッドのみ定義する必要があります。
`Shrinker` は既定では空のシーケンスを返します(すなわち、この型に対してはシュリンクが起こらない)。

そして、このクラスのすべての Arbitrary インスタンスを登録するために、次のようにします。 *)

Arb.register<MyGenerators>()

(**
これで FsCheck は `Tree` 型のことを知るようになりました。そして、Tree 値のみならず、例えば Tree を含むリストやタプル、
オプション値についても生成できます。 *)

(***define-output:RevRevTree***)
let RevRevTree (xs:list<Tree>) = 
  List.rev(List.rev xs) = xs
Check.Quick RevRevTree

(***include-output:RevRevTree***)

(**
ジェネリックな型パラメータを持った型を生成するために、例えば、 *)

type Box<'a> = Whitebox of 'a | Blackbox of 'a

(**
について、同様の原理があてはまります。したがって `MyGenerators` 型は次のように記述できます。 *)

let boxGen<'a> : Gen<Box<'a>> = 
    gen { let! a = Arb.generate<'a>
          return! Gen.elements [ Whitebox a; Blackbox a] }

type MyGenerators =
    static member Tree() =
        {new Arbitrary<Tree>() with
            override x.Generator = tree
            override x.Shrinker t = Seq.empty }
    static member Box() = Arb.fromGen boxGen

(**
`Box` の型パラメータのジェネレータを取得するために、Arb モジュールの関数 `generate<'a>` を使用していることに注目してください。
これによりジェネレータを再帰的に定義することができます。同じように、 `shrink<'a>` 関数があります。
このような Arbitrary インスタンスの記述法の感覚をつかむために、FsCheck のソースコードの既定の Arbitrary 実装の例を参照してください。
Arb モジュールも同様に役立つでしょう。

さあ、次の性質を確認してみましょう。 *)

(***define-output:RevRevBox***)
let RevRevBox (xs:list<Box<int>>) = 
  List.rev(List.rev xs) = xs
Check.Quick RevRevBox

(***include-output:RevRevBox***)

(**
クラスに属性によるタグ付けが必要ないことに注意してください。FsCheck はジェネレータの型を静的メンバの戻り値型により決定します。

また、このケースではジェネレータやシュリンカの記述が必要ないことにも注意してください。FsCheck は、リフレクションにより
判別共用体、レコード型、列挙型に対する適切なジェネレータを作ることができます。

## いくつかの便利な Arb モジュールのメソッド

- `Arb.from<'a>` は与えられた型 'a に対する登録済みの Arbitrary インスタンスを返します。
- `Arb.fromGen` は与えられたジェネレータから新しい Arbitrary インスタンスを作成します。シュリンカは空のシーケンスを返します。
- `Arb.fromGenShrink` は与えられたジェネレータとシュリンカから新しい Arbitrary インスタンスを作成します。これは Arbitrary 自身を実装するのと等価ですが、短くなるでしょう。
- `Arb.generate<'a>` は与えられた型 'a に対する登録済みの Arbitrary インスタンスのジェネレータを返します。
- `Arb.shrink` は与えられた値に対して登録済みの Arbitrary インスタンスの即時シュリンクを返します。
- `Arb.convert` は、変換関数 to ('a -> 'b) と from ('b -> 'a) が与えられると、Arbitrary<'a> インスタンスを Arbitrary<'b> に変換します。
- `Arb.filter` は与えられた Arbitrary インスタンスのジェネレータとシュリンカを、与えられたフィルター関数にマッチする値のみを含むようにフィルターします。
- `Arb.mapFilter` は与えられた Arbitrary インスタンスのジェネレータをマップし、シュリンカをフィルターします。ジェネレータのマッピングは高速になります。例えば、 PositiveInt に対して負の値をフィルターするより絶対値をとったほうが高速です。
- `Arb.Default` は、FsCheck によりあらかじめ公開登録済みのすべての既定の Arbitrary インスタンスを含む型です。これは既定のジェネレータをオーバーロードするのに便利です。通常、ジェネレータからある値をフィルターしたくて、それからオーバーライドしたジェネレータから既定のジェネレータが参照できる必要があるからです。 *)
