## クイックスタート
#### 簡単な例
プロパティ(性質)の定義の簡単な例は
```fsharp
let revRevIsOrig (xs:list<int>) = List.rev(List.rev xs) = xs
```
この性質は、リストを逆順にして逆順にしたものは元のリスト自身になるということを主張します。この性質を確かめるために、F# インタラクティブ上にこの定義をロードして、起動してみましょう。

```fsharp
> Check.Quick revRevIsOrig;;
Ok, passed 100 tests.
```

性質が失敗すると、FsCheck は反例を表示します。例えば、もし
```fsharp
let revIsOrig (xs:list<int>) = List.rev xs = xs
```
を定義すると、
```fsharp
> Check.Quick revIsOrig;;
Falsifiable, after 2 tests (2 shrinks) (StdGen (884019159,295727999)):
[1; 0]
```
という結果を確認できるでしょう。

FsCheck は反例のシュリンク(推定による絞り込みのこと)も行うので、テストケースを失敗させるものが最小の反例となるのです。上の例では反例は確かに最小、つまりリストは少なくとも2つの異なる要素を持たなくてはならないのです。FsCheck はより小さな反例を(何らかの方法で)何回見つけてシュリンクを進めたかを表示します。
#### FsCheck を使う
FsCheck を使うために、最新の FsCheck のソースコードかバイナリをダウンロードします。スペックやテストデータ生成器を含むすべてのプロジェクトにあるアセンブリをビルドし参照します。性質を定義したモジュールを F# インタラクティブにロードし、
```fsharp
Check.Quick <propertyName>
```
を呼び出すか、Check 関数を呼び出す小さなコンソールアプリを書いて実行することで性質をテストできます。xUnit や NUnit のようなテストランナーとの統合も同様に可能です。例は使い方のヒントを御覧ください。
#### 性質をまとめる
大抵は、複数のテストすべき性質を書くことになります。FsCheck はクラスの静的メンバーとして性質をひとまとめにすることができます:
```fsharp
type ListProperties =
    static member ``reverse of reverse is original`` xs = RevRevIsOrig xs
    static member ``reverse is original`` xs = RevIsOrig xs
```
これらは Check.QuickAll 関数を使うことで一度にチェックできます:
```fsharp
> Check.QuickAll<ListProperties>();;
--- Checking ListProperties ---
ListProperties.reverse of reverse is original-Ok, passed 100 tests.
ListProperties.reverse is original-Falsifiable, after 3 tests (3 shrinks) (StdGen (885249229,295727999)):
[1; 0]
```
FsCheck はそれぞれのテスト名も出力します。モジュールにあるすべてのトップレベル関数はそのモジュール名をもつクラスの静的メンバーとしてコンパイルされるので、あるモジュールにあるすべてのトップレベル関数をテストするために Check.QuickAll を使うこともできます。しかし、モジュールの型は F# から直接アクセスできないので、次のようなトリックを使いましょう:
```fsharp
> Check.QuickAll typeof<ListProperties>.DeclaringType;;
--- Checking QuickStart ---
QuickStart.revRevIsOrig-Ok, passed 100 tests.
QuickStart.revIsOrig-Falsifiable, after 6 tests (7 shrinks) (StdGen (885549247,295727999)):
[1; 0]
QuickStart.revRevIsOrigFloat-Falsifiable, after 10 tests (4 shrinks) (StdGen (885679254,295727999)):
[nan]
```
#### もしテストがループしたりエラーに出くわしたら何をする？
性質が有効ではありませんが、Check.Quick が反例を表示しないという場合があります。このような場合のために、別のテスト関数があります。テストを実行する前にそれぞれのテストケースを表示する
```fsharp
Check.Verbose <property_name>
```
を使ってもう一度テストしてみましょう。つまり、最後に表示されたテストケースがループしてるかエラーが発生しているものだということです。Check.VerboseAll は性質のグループをくどくどとチェックするために型やモジュールにも使えます。
#### 警告
上記の性質(逆順の逆順のリストは元のリスト自身)は常に正しいとは限りません。nan (非数(not a number))を含んだ浮動小数点数のリストを考えてみましょう。nan <> nan なので、もし単純に要素同士の比較を用いるなら [nan, nan] の逆順の逆順は実際 [nan, nan] と等しくありません。FsCheck はこの手のスペック問題を見つけ出すコツを備えています。しかし、この振る舞いはめったに思ったとおりにならないので、型多相性(今のところ、unit, bool, char および string 値)を残しているなら FsCheck は「上手く」比較できる値だけを生成します。このエラーを実際に見るために、FsCheck に浮動小数点数のリストを生成させてみましょう:
```fsharp
let revRevIsOrigFloat (xs:list<float>) = List.rev(List.rev xs) = xs

> Check.Quick revRevIsOrigFloat;;
Falsifiable, after 19 tests (12 shrinks) (StdGen (886719313,295727999)):
[nan]
```
## 性質
性質は F# の関数定義として表現されます。性質はそのパラメータにわたって例外なく定量化されるので、
```fsharp
let revRevIsOrig xs = List.rev(List.rev xs) = xs
```
は等式が全てのリスト xs で成り立つことを意味します。性質は決して単相型をもってはいけません。上記のような「多相的な」性質は、まるでジェネリックな引数が object 型であるかのようにテストされます。これは、様々な単純型(bool, char, string, ...)が生成されることを意味します。それは1つの生成されるリストに複数の型が含まれる場合さえあるかもしれないということで、例えば {['r', "1a", true]} は上記の性質をチェックするために使うことができるリストになりえます。ですが生成された値は型をベースにしているので、推論により、あるいは明示的に、xs に異なる型を与えることで、この振る舞いを簡単に変更することもできます。つまり、
```fsharp
let revRevIsOrigInt (xs:list<int>) = List.rev(List.rev xs) = xs
```
は int のリストについてのみチェックされます。FsCheck は様々な形式の性質をチェックすることができます―これらの形式はテスト可能であると呼ばれ、'Testable というジェネリック型によって API において示されます。'Testable は bool または unit を返す任意個のパラメータを取る関数となるでしょう。後者の場合、もし(例外を)送出しなければテストはパスします。
#### 条件付き性質
性質は ==> という形式をとることがあります。例えば、
```fsharp
let rec private ordered xs =
   match xs with
   | [] -> true
   | [x] -> true
   | x::y::ys ->  (x <= y) && ordered (y::ys)

let rec private insert x xs =
   match xs with
   | [] -> [x]
   | c::cs -> if x <= c then x::xs else c::(insert x cs)

let Insert (x:int) xs = ordered xs ==> ordered (insert x xs)
```
条件が成り立つときはいつでも ==> の後の性質が成り立つなら、このような性質は成り立ちます。テストは条件を満たさないテストケースを切り捨てます。条件を満たすケースが100件見つかるまで、あるいはテストケース数の限度に達する(条件が決して成り立たないループを避けるため)まで、テストケースの生成は続けられます。この場合、
```fsharp
Arguments exhausted after 97 tests.
```
というようなメッセージは、条件を満たすテストケースが97件見つかり、その97件に対して性質が成り立った、ということを示します。この場合、生成された値は int 型に制限されなければならなかったことに注意してください。なぜなら生成された値は比較可能である必要がありましたが、これは型に反映されていません。ですから、明示的な制限がなければ、FsCheck は異なる型(object 型の派生型)を含むリストを生成したかもしれず、そしてこれらは互いに比較可能ではありません。
#### 遅延性質
F# はデフォルトでは正則評価ですが、上記の性質は必要以上に働いてしまいます。つまり、左側の条件の結果がどうであれ、条件の右側にある性質を評価します。上の例におけるパフォーマンス面の考察だけではありますが、性質の表現力を制限する可能性があります―考えてみましょう:
```fsharp
let Eager a = a <> 0 ==> (1/a = 1/a)

> Check.Quick Eager;;
Falsifiable, after 1 test (0 shrinks) (StdGen (886889323,295727999)):
0
with exception:
System.DivideByZeroException: Attempted to divide by zero.
   at Properties.Eager(Int32 a) in C:\Users\Kurt\Projects\FsCheck\FsCheck\FsCheck.Documentation\Properties.fs:line 24
   at DocumentationGen.fsCheckDocGen@130-3.Invoke(Int32 a) in C:\Users\Kurt\Projects\FsCheck\FsCheck\FsCheck.Documentation\Program.fs:line 130
   at FsCheck.Testable.evaluate[a,b](FSharpFunc`2 body, a a) in C:\Users\Kurt\Projects\FsCheck\FsCheck\FsCheck\Property.fs:line 168
```
ここで、性質が正しくチェックされているかどうかを確かめるために、遅延評価が必要になります:
```fsharp
let Lazy a = a <> 0 ==> (lazy (1/a = 1/a))

> Check.Quick Lazy;;
Ok, passed 100 tests.
```
#### 定量化された性質
性質は
```fsharp
forAll <arbitrary>  (fun <args> -> <property>)
```
という形式を取る場合があります。例えば、
```fsharp
let InsertWithArb x = forAll orderedList (fun xs -> ordered(insert x xs))
```
forAll の第一引数は IArbitrary インスタンスです。このようなインスタンスはテストデータのジェネレータとシュリンカ(詳しくは後ほど)をカプセル化します。その型のデフォルトであるジェネレータを使う代わりに、お手製のジェネレータを与えることで、テストデータの分布をコントロールすることができます。この例では、整序済みリスト用の自作ジェネレータを与えることで、整序されていないテストケースをフィルタリングするというよりも、むしろテストケースの総合的な限界に達することなく100のテストケースを生成できることを保証します。ジェネレータを定義するためのコンビネータを後ほど説明します。
#### 例外の予測
ある状況下で関数やメソッドが例外をスローするというテストをしたいと思うかもしれません。次のコンビネータが役に立ちます:
```fsharp
throws<'e :> exn,'a> Lazy<'a>
```
例:
```fsharp
let ExpectDivideByZero() = throws<DivideByZeroException,_> (lazy (raise <| DivideByZeroException()))

> Check.Quick ExpectDivideByZero;;
Ok, passed 100 tests.
```
#### 時間制限のある性質
性質は
```fsharp
within <timeout in ms> <Lazy<property>>
```
という形式をとることがあります。例えば、
```fsharp
let timesOut (a:int) =
    lazy
        if a>10 then
            while true do System.Threading.Thread.Sleep(1000)
            true
        else
            true
    |> within 2000

> Check.Quick timesOut;;
Timeout of 2000 milliseconds exceeded, after 37 tests (0 shrinks) (StdGen (945192658,295727999)):
11
```
第一引数は与えられた遅延性質が実行してよい最大の時間です。もしそれよりも長く実行していると、FsCheck はそのテストが失敗したと見做します。そうでなければ、遅延性質の結果は within の結果です。within が性質が実行されているスレッドをキャンセルしようとしても、それはきっと上手くいかないでしょうし、そのスレッドはプロセスが終了するまで実際に実行し続けるであろうということに注意してください。
#### テストケース分布の観測
テストケースの分布を意識しておくのは重要なことです。つまり、もしテストデータがよく分布していなかったら、テスト結果から導き出される結論は正しくないかもしれません。特に、与えられた性質を満足するテストデータだけが使われるので、==> 演算子はテストデータの分布を不適切に歪めます。FsCheck はテストデータの分布を観測する手段を幾つか提供します。観測するためのコードは性質の宣言に含まれ、性質が実際にテストされる度に観測され、収集した観測結果はテストが完了した時に集約されます。
#### 自明なケースの計数
性質は
```fsharp
trivial <condition> <property>
```
という形式をとることがあります。例えば、
```fsharp
let insertTrivial (x:int) xs =
    ordered xs ==> (ordered (insert x xs))
    |> trivial (List.length xs = 0)
```
この条件が真になるテストケースは自明であると分類され、全体における自明なテストケースの比率が報告されます。この例では、テストすることで
```fsharp
> Check.Quick insertTrivial;;
Arguments exhausted after 55 tests (36% trivial).
```
という結果になります。
#### テストケースの分類
性質は classify という形式をとることがあります。例えば、
```fsharp
let insertClassify (x:int) xs =
   ordered xs ==> (ordered (insert x xs))
   |> classify (ordered (x::xs)) "at-head"
   |> classify (ordered (xs @ [x])) "at-tail"
```
条件を満足するテストケースは与えられた分類に割り当てられ、分類の分布はテスト後に報告されます。この場合、結果は
```fsharp
> Check.Quick insertClassify;;
Arguments exhausted after 54 tests.
44% at-tail, at-head.
24% at-head.
22% at-tail.
```
1つのテストケースは複数の分類に当てはまる場合があることに注意してください。
#### データの値の収集
性質は
```fsharp
collect <expression> <property>
```
という形式をとることがあります。例えば、
```fsharp
let insertCollect (x:int) xs =
    ordered xs ==> (ordered (insert x xs))
    |> collect (List.length xs)
```
collect の引数はテストケース毎に評価され、値の分布が報告されます。この引数の型は sprintf "%A" を用いて出力されます。上記の例では、出力は
```fsharp
> Check.Quick insertCollect;;
Arguments exhausted after 70 tests.
50% 0.
32% 1.
11% 2.
4% 3.
1% 4.
```
です。
#### 観測の連結
ここで説明した観測は何らかの方法で連結されるかもしれません。テストケースそれぞれにおける全ての観測は連結されており、これらの連結の分布は報告されます。例えば、
```fsharp
let insertCombined (x:int) xs =
    ordered xs ==> (ordered (insert x xs))
    |> classify (ordered (x::xs)) "at-head"
    |> classify (ordered (xs @ [x])) "at-tail"
    |> collect (List.length xs)
```
という性質をテストすると、
```fsharp
> Check.Quick insertCombined;;
Arguments exhausted after 53 tests.
24% 0, at-tail, at-head.
18% 1, at-tail, at-head.
18% 1, at-head.
16% 1, at-tail.
7% 2, at-head.
5% 2, at-tail.
3% 2.
1% 5, at-tail.
1% 4.
```
となります。
#### And、Or および従属性質へのラベル付け
性質は
```fsharp
<property> .&. <property>

<property> .|. <property>
```
という形式をとることがあります。

p1 .&. p2 は両方成功した場合に成功し、性質のどちらか一方が失敗した場合に失敗し、両方ともに棄却された場合に棄却されます。p1 .|. p2 は性質のどちらかが成功した場合に成功し、両方の性質が失敗した場合に失敗し、両方とも棄却された場合に棄却されます。.&. コンビネータは、ジェネレータを共有する複雑な性質を記述するために一般に最も使われます。この場合、失敗時にどのサブプロパティ(従属性質)が原因で失敗したか正確に知るのが難しいことがあるかもしれません。そのために従属性質にラベルを付けることができて、FsCheck が反例を見つけると、失敗した従属性質のラベルを表示します。これはこのような形になります:
```fsharp
<string> @| <property>

<property> |@ <string>
```
例えば、
```fsharp
let complex (m: int) (n: int) =
    let res = n + m
    (res >= m)    |@ "result > #1" .&.
    (res >= n)    |@ "result > #2" .&.
    (res < m + n) |@ "result not sum"
```
はこのようになります:
```fsharp
> Check.Quick complex;;
Falsifiable, after 1 test (0 shrinks) (StdGen (995775551,295727999)):
Label of failing property: result not sum
0
0
```
1つの性質に複数のラベルを適用することは一向に構いません。FsCheck は適用可能なすべてのラベルを表示します。これは途中の結果を表示するのに便利で、例えば:
```fsharp
let multiply (n: int, m: int) =
    let res = n*m
    sprintf "evidence = %i" res @| (
    "div1" @| (m <> 0 ==> lazy (res / m = n)),
    "div2" @| (n <> 0 ==> lazy (res / n = m)),
    "lt1"  @| (res > m),
    "lt2"  @| (res > n))

> Check.Quick multiply;;
Falsifiable, after 1 test (0 shrinks) (StdGen (996145572,295727999)):
Labels of failing property: evidence = 0, lt1
(0, 0)
```
上記の性質は従属性質をタプルにすることで連結していることに注意しましょう。これは長さ 6 のタプルまで上手くいきます。リストに対しても上手くいきます。一般的な形式
```fsharp
(<property1>,<property2>,...,<property6>) means <property1> .&. <property2> .&.... .&.<property6>

[property1;property2,...,propertyN] means <property1> .&. <property2> .&.... .&.<propertyN>
```
リストとして記述した例:
```fsharp
let multiplyAsList (n: int, m: int) =
    let res = n*m
    sprintf "evidence = %i" res @| [
    "div1" @| (m <> 0 ==> lazy (res / m = n));
    "div2" @| (n <> 0 ==> lazy (res / n = m));
    "lt1"  @| (res > m);
    "lt2"  @| (res > n)]
```
同じ結果となります。
## テストデータ: ジェネレータ、シュリンカ、 Arbitrary インスタンス
テストデータはテストデータジェネレータによって作成されます。 FsCheck には、いくつかのよく使用される型については既定のジェネレータが定義されていますが、自分で定義することもできますし、導入した新しい型については自身でジェネレータを定義する必要があります。ジェネレータは Gen<'a> という形式の型を持ちます。これは型 a の値のためのジェネレータです。型 Gen の値を扱うために、 FsCheck は gen と呼ばれるコンピュテーション式を提供します。これにより Gen モジュール内のすべての関数が自由に利用できます。シュリンカは 'a -> seq<'a> という型を持ちます。値を一つ受け取ると、ある方法により受け取った値より小さな値からなるシーケンスを生成します。 FsCheck は、与えられた性質を満たさない値セットを見つけると、値のシュリンクを取得して、それぞれを順番に試して性質が満たされないことを確認することにより、元の（ランダムな）値より小さな値を作ろうとします。もし存在すれば、見つかった小さな値が新しい反例となり、その値とともにシュリンク過程が継続します。シュリンカに FsCheck からの特別なサポートはありません。言い換えればこれは必要ないということです。なぜならば、必要なものはすべて seq コンピュテーション式と Seq モジュールにあるからです。最後に、 Arbitrary<'a> インスタンスは、性質で使われるようにこれら 2 つの型をまとめます。 FsCheck では Arbitrary インスタンスを、 Type から Arbitrary インスタンスの辞書に登録することもできます。この辞書は、引数のタイプに基づき、引数を持つ性質の任意のインスタンスを見つけるために使われます。 Arb モジュールには Arbitrary インスタンスのためのヘルパ関数があります。
#### ジェネレータ
ジェネレータは、関数
```fsharp
val choose : (int * int -> Gen<int>)
```
により構築されます。これは一様分布を利用した区間から値のランダム抽出を行います。例えばリストからの要素のランダム抽出を行いたい場合、
```fsharp
let chooseFromList xs = 
    gen { let! i = Gen.choose (0, List.length xs-1) 
          return (List.nth xs i) }
```
を使用します。
#### 選択肢から選ぶ
ジェネレータは、リスト内のジェネレータから等確率で選ぶ Gen.oneof のような形式をとるかもしれません。例えば、
```fsharp
Gen.oneof [ gen { return true }; gen { return false } ]
```
は確率 1/2 で true になるランダムな bool 値を生成します。あるいは、関数
```fsharp
val frequency: seq<int * Gen<'a>> -> Gen<'a>
```
を使用して、結果の分布を制御することができます。 frequency はリストからジェネレータをランダムに選びますが、それぞれの選択肢が選ばれる確率は与えられた要素により重み付けされます。例えば、
```fsharp
Gen.frequency [ (2, gen { return true }); (1, gen { return false })]
```
は 2/3 の確率で true を生成します。
#### テストデータサイズ
テストデータジェネレータは暗黙のサイズパラメータを持ちます。 FsCheck は小さなテストケースを作ることから始め、テストが進むにつれて徐々にサイズを増加させます。テストデータジェネレータによってサイズパラメータの解釈の方法が異なります。あるものはそれを無視しますが、例えばリストジェネレータはそれを生成されるリストの長さの上限として解釈します。使用するかどうかは、テストデータジェネレータを制御したいかどうかによります。サイズパラメータの値は
```fsharp
val sized : ((int -> Gen<'a>) -> Gen<'a>)
```
を使用することで取得できます。 sized g は、現在のサイズをパラメータとして g を呼びます。例えば、 0 から大きさまでの自然数を生成するには、
```fsharp
Gen.sized <| fun s -> Gen.choose (0,s)
```
を使用します。

サイズ制御の目的は、エラーを見つけるのに充分大きく、しかもテストを素早く行うのに充分小さいテストケースを確保するためにあります。既定のサイズ制御ではこれを達成できない場合があります。例えば、 1 回のテストランの終わりまでに任意のリストは 50 要素まで作られますが、これはつまりリストのリストは 2,500 要素となり、効率的なテストとしては大きすぎます。このようなケースでは、明示的にサイズパラメータを変更するのが良いでしょう。そのようにするには
```fsharp
val resize : (int -> Gen<'a> -> Gen<'a>)
```
を使用します。 resize n g はサイズパラメータ n とともに g を実行します。サイズパラメータは負になってはいけません。例えば、ランダム行列を生成するために元のサイズの平方根をとるのは適切でしょう。
```fsharp
let matrix gen = Gen.sized <| fun s -> Gen.resize (s|>float|>sqrt|>int) gen
```
#### 再帰的データ型の生成
再帰的データ型のためのジェネレータは、コンストラクタを選択するために oneof あるいは frequency を、また各ケース用のジェネレータを作るために F# の標準的なコンピュテーション式の構文を使用することで、容易に表現することができます。また、 6 個までのアリティをコンストラクタや関数を Gen 型にマップする関数もあります。例えば、木の型が
```fsharp
type Tree = Leaf of int | Branch of Tree * Tree
```
で定義されているとすると、木のためのジェネレータは
```fsharp
let rec unsafeTree() = 
    Gen.oneof [ Gen.map Leaf Arb.generate<int> 
                Gen.map2 (fun x y -> Branch (x,y)) (unsafeTree()) (unsafeTree())]
```
で定義できるでしょう。しかしながら、このような再帰ジェネレータはおそらく停止できずに StackOverflowException とともに失敗するか、とても大きな結果を生じるでしょう。これを防ぐために、再帰ジェネレータは常にサイズ制御機序を使用するべきです。例えば、
```fsharp
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
```
以下に注意してください。

* サイズが 0 であるときに結果が葉になることを強制することで停止を保証しています。
* 再帰ごとにサイズを半分にします。これによりサイズは木におけるノード数の上限をとなります。解釈したいようにサイズを自由に解釈できます。
* 2 つの枝間で 1 つの部分木を共有しているという事実は、ケースごとに同じ木を生成するということを意味しません。

#### 便利なジェネレータコンビネータ
g を型 t に対するジェネレータとすると、
- two g は t の 2 つ組を生成します。
- three g は t の 3 つ組を生成します。
- four g は t の 4 つ組を生成します。
- xs をリストとすると、 elements xs は xs の任意の要素を生成します。
- listOfLength n g はちょうど n 個の t のリストを生成します。
- listOf g は長さがサイズパラメータによって決められる t のリストを生成します。
- nonEmptyListOf g は長さがサイズパラメータによって決められる t の空でないリストを生成します。
- constant v は値 v を生成します。
- suchThat p g は述語 p を満足する t を生成します。述語を満たす確率が高いようにしてください。
- suchThatOption p g は述語 p を満足するならば Some t を、見つからなければ None を生成します（「一生懸命」やった後で）。

これらすべてのジェネレータコンビネータは Gen モジュールの関数です。
#### 型に基づく既定のジェネレータとシュリンカ
FsCheck には、よく使用される型（unit、 bool、 byte、 int、 float、 char、 string、 DateTime、リスト、一次元および二次元の配列、 Set、 Map、オブジェクト、以上の型から型への関数）については既定のテストデータジェネレータとシュリンカが定義されています。さらに、リフレクションを用いて、 FsCheck はあらゆるプリミティブ型に関して（FsCheck 内に、あるいはユーザーによって）定義されたレコード型、判別共用体、タプル、列挙型の既定の実装を得ることができます。これらについては性質ごとに明示的に定義する必要はありません。 FsCheck は、すべての性質の引数について、もしジェネレータやシュリンカを知っていたり、得られたりするならば、適切なジェネレータとシュリンカを指定したプロパティを提供することができます。たいていの場合、性質に基づいたこれらの型を見つけ出す仕事を型推論に任せることができます。しかしながら FsCheck に特定のジェネレータとシュリンカを強制したい場合は、適切な型注釈を与えることによって可能になります。導入で述べたように、 FsCheck は特定の型に対するジェネレータとシュリンカを Arbitrary 型としてまとめることができます。 Arbitrary<'a> を継承したクラスのインスタンスを返すようなクラスの静的メンバを定義することで、独自型に対する Arbitrary インスタンスを FsCheck に与えることができます。
```fsharp
type MyGenerators =
    static member Tree() =
        {new Arbitrary<Tree>() with
            override x.Generator = tree
            override x.Shrinker t = Seq.empty }
```
'a を Arbitrary インスタンスを定義したい特定の型に置き換えてください。 Generator メソッドのみ定義する必要があります。 Shrinker は既定では空のシーケンスを返します（すなわち、この型に対してはシュリンクが起こらない）。そして、このクラスのすべての Arbitrary インスタンスを登録するために、次のようにします。
```fsharp
Arb.register<MyGenerators>()
```
これで FsCheck は Tree 型のことを知るようになりました。そして、 Tree 値のみならず、例えば Tree を含むリストやタプル、オプション値についても生成できます。
```fsharp
let RevRevTree (xs:list<Tree>) = 
List.rev(List.rev xs) = xs

> Check.Quick RevRevTree;;
Ok, passed 100 tests.
```
ジェネリックな型パラメータを持った型を生成するために、例えば、
```fsharp
type Box<'a> = Whitebox of 'a | Blackbox of 'a
```
について、同様の原理があてはまります。したがって MyGenerators 型は次のように記述できます。
```fsharp
let boxGen<'a> : Gen<Box<'a>> = 
    gen { let! a = Arb.generate<'a>
          return! Gen.elements [ Whitebox a; Blackbox a] }

type MyGenerators =
    static member Tree() =
        {new Arbitrary<Tree>() with
            override x.Generator = tree
            override x.Shrinker t = Seq.empty }
    static member Box() = Arb.fromGen boxGen
```
Box の型パラメータのジェネレータを取得するために、 Arb モジュールの関数 'val generate<'a> : Gen<'a>' を使用していることに注目してください。これによりジェネレータを再帰的に定義することができます。同じように、 shrink<'a> 関数があります。このような Arbitrary インスタンスの記述法の感覚をつかむために、 FsCheck のソースコードの既定の Arbitrary の実装の例を参照してください。 Arb モジュールも同様に役立つでしょう。さあ、次の性質を確認してみましょう。
```fsharp
let RevRevBox (xs:list<Box<int>>) = 
    List.rev(List.rev xs) = xs
    |> Prop.collect xs

> Check.Quick RevRevBox;;
Ok, passed 100 tests.
11% [].
2% [Blackbox 0].
1% [Whitebox 9; Blackbox 3; Whitebox -2; Blackbox -8; Whitebox -13; Blackbox -19;
(etc)
```
クラスに属性によるタグ付けが必要ないことに注意してください。 FsCheck はジェネレータの型を静的メンバの戻り値型により決定します。また、このケースではジェネレータやシュリンカの記述が必要ないことにも注意してください。 FsCheck は、リフレクションにより判別共用体、レコード型、列挙型に対する適切なジェネレータを作ることができます。
#### いくつかの便利な Arb モジュールのメソッド
- Arb.from<'a> は与えられた型 'a に対する登録済みの Arbitrary インスタンスを返します。
- Arb.fromGen は与えられたジェネレータから新しい Arbitrary インスタンスを作成します。 シュリンカは空のシーケンスを返します。
- Arb.fromGenShrink は与えられたジェネレータとシュリンカから新しい Arbitrary インスタンスを作成します。 これは Arbitrary を自身で実装するのと等価ですが、短くなるでしょう。
- Arb.generate<'a> は与えられた型 'a に対する登録済みの Arbitrary インスタンスのジェネレータを返します。
- Arb.shrink は与えられた値に対して登録済みのインスタンスの即時シュリンクを返します。
- Arb.convert は、変換関数 to ('a ->'b) と from ('b ->'a) が与えられると、 Arbitrary<'a> インスタンスを Arbitrary<'b> に変換します。
- Arb.filter は与えられた Arbitrary インスタンスのジェネレータとシュリンカを、与えられたフィルター関数にマッチする値のみを含むように、フィルターします。
- Arb.mapFilter は与えられた Arbitrary インスタンスのジェネレータをマップし、シュリンカをフィルターします。 ジェネレータのマッピングは高速になります。例えば PositiveInt に対して負の値をフィルターするより絶対値をとった方が高速です。
- Arb.Default は、 FsCheck によりあらかじめ公開登録済みのすべての既定の Arbitrary インスタンスを含む型です。 これは既定のジェネレータをオーバーライドするのに便利です。 通常、ジェネレータからある値をフィルターしたくて、それからオーバーライドしたジェネレータから既定のジェネレータが参照できる必要があるからです。

## 状態遷移を伴うテストの実行
たいていの場合オブジェクトは一連のメソッドによって内部状態がカプセル化されているわけですが、FsCheckではこのようなオブジェクトを対象にしたテストを実行することもできます。 FsCheckではほんの少し手を加えるだけで、テスト対象となるクラスに対してモデルを基準とするような仕様を定義できます。 たとえば以下のような人為的なバグが含まれているクラスがあるとします：
```fsharp
type Counter() =
    let mutable n = 0
    member x.Inc() = n <- n + 1
    member x.Dec() = if n > 2 then n <- n - 2 else n <- n - 1
    member x.Get = n
    member x.Reset() = n <- 0
    override x.ToString() = n.ToString()
```
このクラスをテストするためのモデルとしては、オブジェクトの内部状態を表すのに役立ちそうなint値1つがふさわしいでしょう。 それを踏まえると、以下のような仕様が作成できます：
```fsharp
let spec =
    let inc =
        { new ICommand<Counter,int>() with
            member x.RunActual c = c.Inc(); c
            member x.RunModel m = m + 1
            member x.Post (c,m) = m = c.Get
            override x.ToString() = "inc" }
    let dec =
        { new ICommand<Counter,int>() with
            member x.RunActual c = c.Dec(); c
            member x.RunModel m = m - 1
            member x.Post (c,m) = m = c.Get
            override x.ToString() = "dec" }
    { new ISpecification<Counter,int> with
        member x.Initial() = (new Counter(),0)
        member x.GenCommand _ = Gen.elements [inc;dec] }
```
仕様はISpecification<'typeUnderTest,'modelType>を実装したオブジェクトです。 仕様は、初期状態のオブジェクトと、オブジェクトに対する初期状態のモデルを返さなくてはいけません。 また、ICommandオブジェクトのジェネレーターも返す必要があります。 それぞれのICommandオブジェクトでは、一般的にはテスト対象のオブジェクトに対する1つのメソッド呼び出しに対応するようにして、 コマンドを実行することでモデルとオブジェクトに対して起こることを定義します。 また、コマンドを実行する前に満たすべき事前条件をアサートします。 すなわち、もしも前提条件が一致しないのであればFsCheckはそのコマンドを実行しません。 コマンドの実行後は一致すべき事後条件もチェックされます。 すなわち、事後条件が一致しない場合、FsCheckではテストが失敗したものと判断されます。 なお反例が表示できるようにToStringをオーバーライドしておくとよいでしょう。 仕様を以下のようにチェックできます：
```fsharp
> Check.Quick (asProperty spec);;
Falsifiable, after 6 tests (2 shrinks) (StdGen (1020916989,295727999)):
[inc; inc; inc; dec]
```
FsCheckは「バグ」を発見しただけでなく、バグを発生させる最小のシーケンスも生成したことにも注目してください。
## 使い方のヒント
#### 関数の性質
FsCheck はランダムな関数値を生成できるので、関数の性質を検査できます。例えば、次のように関数合成の結合性を検査できます。
```fsharp
let associativity (x:Tree) (f:Tree->float,g:float->char,h:char->int) = ((f >> g) >> h) x = (f >> (g >> h)) x

> Check.Quick associativity;;
Ok, passed 100 tests.
```
Tree -> 任意の型 の関数を生成できます。反例が見つかった場合は、関数値が "func" として表示されます。しかしながら、 FsCheck は Function 型を使用することで、より詳細に生成された関数を表示することができます。もしそれを使えば、 FsCheck は関数をシュリンクことさえ可能です。例は以下の通りです。
```fsharp
let mapRec (F (_,f)) (l:list<int>) =
    not l.IsEmpty ==>
        lazy (List.map f l = ((*f <|*) List.head l) :: List.map f (List.tail l))

> Check.Quick mapRec;;
Falsifiable, after 1 test (1 shrink) (StdGen (1028557426,295727999)):
{ 0->0; 1->0; 2->0 }
[1]
```
type Function<'a,'b> = F of ref<list<('a*'b)>> * ('a ->'b) は呼ばれていたすべての引数、および生成した結果の写像を記録します。性質では、例のように、パターンマッチによって実際の関数を抽出することができます。 Function は関数を出力し、また、関数をシュリンクするために使用されます。
#### カスタムジェネレータを使用する forAll の代わりにパターンマッチを使用する
```fsharp
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
```
特にカスタムシュリンク関数と同様に定義できるので、これは性質をはるかに読みやすくします。 FsCheck は、例えば NonNegativeInt 、 PositiveInt 、 StringWithoutNullChars など、 FsCheck はこれらのうちかなりの数を持っています。 Arbitrary のデフォルトインスタンスである Arb.Default 型を見てみましょう。そしてそのうえ、あなた自身で定義したい場合は、 Arb.filter 、 Arb.convert 、Arb.mapFilter が役立つでしょう。
#### モジュールのみを使ったテスト実行
Arbitrary インスタンスはクラスの静的メンバとして与えられ、性質はクラスの静的メンバとして一緒にグループ化することができるので、また、トップレベルの let 関数はそれらを囲むモジュール(クラスとしてコンパイルされる)の静的メンバとしてコンパイルされるので、単に性質やジェネレータをトップレベルの let-束縛関数として定義でき、次のトリックを使用してすべてのジェネレータと性質を登録することができます。
```fsharp
let myprop =....
    let mygen =...
    let helper = "a string"
    let private helper' = true

type Marker = class end
    Arb.register (typeof<Marker>.DeclaringType)
    Check.All (typeof<Marker>.DeclaringType)
```
Marker 型は、モジュールの Type が取得できるように、モジュール内に定義されているだけの任意の型です。 F# は直接モジュールの型を取得する方法を提供していません。 FsCheck は、戻り値の型に基づいて関数の意図を決定しています。すなわち、
* 性質： unit、bool、Property、これらの型への任意の引数の関数、もしくはこれらの型のいずれかの Lazy 値を返します
* Arbitrary インスタンス： Arbitrary<_> を返します

他の全ての関数は丁重に無視されます。もし FsCheck が何かを行うという型を返す関数をトップレベルに持っていて、しかしそれらをチェックしたり登録したくない場合は、単にそれらを private にするだけです。 FsCheck はこれらの関数を無視します。
#### あなたのテストのために働く NUnit Addin を取得するための一時修正

テスト(FsCheck.NUnit の [Property] でマークされたメソッド)を含むプロジェクトでは、以下のことを行ってください。

* FsCheck.Nunit と FsCheck.NUnit.Addin を参照に追加
* アドインを実装するプロジェクトに public クラスを追加

ここに、 F# の例を示します。
```fsharp
open NUnit.Core.Extensibility
open FsCheck.NUnit
open FsCheck.NUnit.Addin
[<NUnitAddin(Description = "FsCheck addin")>]
type FsCheckAddin() =
    interface IAddin with
        override x.Install host =
            let tcBuilder = new FsCheckTestCaseBuider()
            host.GetExtensionPoint("TestCaseBuilders").Install(tcBuilder)
            true
```
これは C# プロジェクトでも使用できます。
```csharp
using FsCheck.NUnit.Addin;
using NUnit.Core.Extensibility;
namespace FsCheck.NUnit.CSharpExamples
{
    [NUnitAddin(Description = "FsCheck addin")]
    public class FsCheckNunitAddin : IAddin
    {
        public bool Install(IExtensionHost host)
        {
            var tcBuilder = new FsCheckTestCaseBuider();
            host.GetExtensionPoint("TestCaseBuilders").Install(tcBuilder);
            return true;
        }
    }
}
```
その後は、このように [Test] の代わりに [Property] を使用してテストにフラグを宣言することができます。
```fsharp
[<Property>]
let maxLe (x:float) y =
    (x <= y) ==> (lazy (max  x y = y))
```
アドインは、 Check.One を使ってこれを実行し、 テストに失敗した場合に、そのフラグを立てるために Assert.Fail を実行します。
#### FsCheck と mb|x|N|cs|Unit を統合するための IRunner 実装
Check.One もしくは Check.All メソッドに渡すことができる Config 型は、引数として IRunner をとります。このインターフェイスは次のメソッドを持っています。

* OnStartfixture は、FsCheck がその型のすべてのメソッドをテストするときに、任意のテストを開始する前に呼び出されます。
* OnArguments は、テスト番号、引数、あらゆる関数の実装を渡して、全てのテストの後に呼び出されます。
* OnShrink は全ての成功したシュリンクに対して呼び出されます。
* OnFinished は、テストの名前と、全体的なテスト実行の結果を伴って呼び出されます。これは以下の例のように、外側のユニットテストフレームワークから Assert 文を呼び出すために使われます - FsCheck は複数のユニットテストフレームワークを統合することができます。あなたは、 setup や tear down、素敵なグラフィカルランナーなどを持つ他のユニットテスティングフレームワークの能力に影響力を行使することができます。

```fsharp
let xUnitRunner =
    { new IRunner with
        member x.OnStartFixture t = ()
        member x.OnArguments (ntest,args, every) = ()
        member x.OnShrink(args, everyShrink) = ()
        member x.OnFinished(name,testResult) =
            match testResult with
            | TestResult.True _ -> Assert.True(true)
            | _ -> Assert.True(false, Runner.onFinishedToString name result)
    }

let withxUnitConfig = { Config.Default with Runner = xUnitRunner }
```
#### 生成された引数の出力をカスタマイズするための IRunner 実装
デフォルトでは、 FsCheck は sprintf "%A"、すなわち構造化フォーマットを使用して生成された引数を出力します。これは通常、あなたの期待通りのことを行います。すなわち、プリミティブ型は値を出力し、オブジェクトは ToString オーバーライドを出力するなどです。もしそうしなければ（動機のあるケースは、 COM オブジェクトをテストすることです - オーバーライドされた ToString という選択肢はなく、構造化フォーマットはそれに役立つようなことは何もしません）、これを性質毎に解決するためにラベルコンビネータを使用することができますが、より構造化した解決策は IRunner を実装することで達成することができます。例は以下の通りです。
```fsharp
let formatterRunner =
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
                                    TestResult.False (testData,origArgs |> List.map formatter,
                                                      shrunkArgs |> List.map formatter,outCome,seed)
                                | t -> t
            printf "%s" (Runner.onFinishedToString name testResult')
    }
```
#### 等式の左辺と右辺を出力する等価比較
性質は、一般に等価性をチェックします。テストケースが失敗した場合、FsCheck は反例を出力しますが、時に、最初に生成された引数でいくつかの複雑な計算を行う場合は特に、比較の左辺と右辺も出力すると便利です。これを簡単にするために、独自のラベル表示等価コンビネータを定義できます。
```fsharp
let (.=.) left right = left = right |@ sprintf "%A = %A" left right

let testCompare (i:int) (j:int) = 2*i+1  .=. 2*j-1

> Check.Quick testCompare;;
Falsifiable, after 1 test (0 shrinks) (StdGen (1029127459,295727999)):
Label of failing property: 1 = -1
0
0
```
もちろん、あなたがよく使用する任意の演算子や関数のためにこれを行うことができます。
#### FsCheck を使用するためのいくつかの方法
* あなたのプロジェクトの fsx ファイルに性質やジェネレータを追加することで。実行することは簡単で、 ctrl-a を入力してから alt-enter を入力すると、結果は F# Interactive に表示されます。ソリューションに組み込まれている dll を参照する時は注意してください。 F# interactive はセッションの残りの間はずっとそれらをロックし、あなたがセッションを終了するまでビルドすることができません。一つの解決策は、 dll の代わりにソースファイルを(ソリューションに)含めることですが、それは処理が遅くなります。小規模なプロジェクトに有用です。私が知る限りでは、デバッグは困難です。
* 別のコンソールアプリケーションを作成することで。アセンブリに迷惑なロックを行わないので、デバッグは簡単です。 テストのために FsCheck のみをを使用し、性質が複数のアセンブリをまたぐような場合は、最良の選択肢です。
* 別のユニットテストフレームワークを使用することで。 FsCheck / ユニットテストの手法が混在し(いくつかのものはユニットテストを使用して調べた方が簡単であり、逆もまた然り)、グラフィカルランナーを好む場合に便利です。あなたが使用しているユニットテストフレームワーク次第では、無料で Visual Studio と上手く統合できるでしょう。このシナリオで FsCheck をカスタマイズする方法は、上記を参照してください。

- - -

### 翻訳者 / translated by
* [@Gab-km](https://github.com/Gab-km)
* [@kos59125](https://github.com/kos59125)
* [@yukitos](https://github.com/yukitos)
* [@pocketberserker](https://github.com/pocketberserker)
