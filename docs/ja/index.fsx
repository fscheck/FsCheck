(*** hide ***)
// このコードブロックは生成された HTML ドキュメントでは省略されます。ドキュメントで
// 見せたくない補助的なものを定義するために使います。
#I "../../src/FsCheck/bin/Release/netstandard2.0"

(**
FsCheck
=======

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      FsCheck と xUnit.NET プラグインは <a href="https://www.nuget.org/packages?q=fscheck">NuGet からインストール</a> 出来ます:
      <pre>PM> Install-Package FsCheck
PM> Install-Package FsCheck.Nunit
PM> Install-Package FsCheck.Xunit</pre>
    </div>
  </div> 
  <div class="span1"></div>
</div>

ドキュメント
-------------

 * [クイックスタート](QuickStart.html)から始めましょう。

 * [性質](Properties.html)はテストを表現する FsCheck の言語を説明します —
   別のフレームワークでは、パラメタライズドテストや生成的テストと呼ばれることもあります。
   FsCheck はこれらを性質と呼びます。

 * [テストデータの生成](TestData.html)は FsCheck により良いデータを生成させたり、
   テストしようとしている対象に対して意味のないデータの生成をやめさせたりする方法を説明します。
   FsCheck はテスト値の生成器(ジェネレーター)や収縮器(シュリンカー)を表現し、
   それらをあなたの書いた性質に適用するための柔軟な言語を備えています。

 * [モデルベースのテスト](StatefulTesting.html)は FsCheck がオブジェクトやデータに対する
   大量のランダムな操作を生成し、各操作の結果は(ずっと単純な)モデルを用いて比較されるという、
   特徴的なテストのアプローチです。

 * [テストの実行](RunningTests.html)は FsCheck を実行するための様々な方法や
   ユニットテストフレームワークとの統合のしかたを説明します。

 * [使いかたのヒント](TipsAndTricks.html) 

 * [API リファレンス](../reference/index.html)は全ての型、モジュール、関数に対する自動生成されたドキュメントを含みます。
 
貢献と著作権
------------

本プロジェクトは[issue の報告][issues]、プロジェクトのフォーク、そしてプルリクエストの送信を行うことが
出来る [GitHub][gh] にホストされています。もし新しいパプリックな API を追加しようとしているなら、
ドキュメントになる[サンプル][content]を追加することも考慮に入れておいてください。

本ライブラリは BSD ライセンスの下で利用可能であり、変更および営利・非営利目的両方の再頒布が許可されます。
詳細については、GitHub のリポジトリにある[ライセンスファイル][license]を確認して下さい。

  [content]: https://github.com/fscheck/FsCheck/tree/master/docs/content
  [gh]: https://github.com/fscheck/FsCheck
  [issues]: https://github.com/fscheck/FsCheck/issues
  [readme]: https://github.com/fscheck/FsCheck/blob/master/README.md
  [license]: https://github.com/fscheck/FsCheck/blob/master/License.txt
*)
