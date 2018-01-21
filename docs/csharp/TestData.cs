using FsCheck;
using System;

namespace CSharp.DocSnippets {

    public abstract class Tree {

    }

    public class Leaf : Tree {
        public int Value { get; private set; }

        public Leaf(int value) {
            Value = value;
        }
    }

    public class Branch : Tree {
        public Tree Left { get; private set; }
        public Tree Right { get; private set; }

        public Branch(Tree left, Tree right) {
            Left = left;
            Right = right;
        }
    }

    class TestData {

        //[chooseFrom]
        public Gen<T> ChooseFrom<T>(T[] xs) {
            return from i in Gen.Choose(0,xs.Length-1)
                   select xs[i];
        }
        //[/chooseFrom]

        //[matrixGen]
        public static Gen<T> Matrix<T>(Gen<T> generator) {
            return Gen.Sized(s => generator.Resize(Convert.ToInt32(Math.Sqrt(s))));
        }
        //[/matrixGen]

        //[unsafeTree]
        public static Gen<Tree> UnsafeTree() {
            return Gen.OneOf(Arb.Generate<int>().Select(i => (Tree) new Leaf(i)),
                             Gen.Two(UnsafeTree()).Select(t => (Tree) new Branch(t.Item1,t.Item2)));
        }
        //[/unsafeTree]

        //[safeTree]
        public static Gen<Tree> SafeTreeHelper(int size) {
            if (size == 0) {
                return Arb.Generate<int>().Select(i => (Tree)new Leaf(i));
            }
            else {
                var subtree = SafeTreeHelper(size / 2);
                return Gen.OneOf(Arb.Generate<int>().Select(i => (Tree) new Leaf(i)),
                                 Gen.Two(subtree).Select(t => (Tree) new Branch(t.Item1,t.Item2)));
            }
        }

        public static Gen<Tree> SafeTree() {
            return Gen.Sized(SafeTreeHelper);
        }
        //[/safeTree]

        //[MyGenerators]
        public class MyGenerators {
            public static Arbitrary<Tree> Trees() {
                return Arb.From(SafeTree());
            }
        }
        //[/MyGenerators]

        public static void Samples() {
            //[chooseBool]
            var chooseBool = Gen.OneOf(Gen.Constant(true), Gen.Constant(false));
            //[/chooseBool]

            //[chooseBool2]
            var chooseBool2 = Gen.Frequency(
                Tuple.Create(2, Gen.Constant(true)),
                Tuple.Create(1, Gen.Constant(false)));
            //[/chooseBool2]

            //[sizedInt]
            var sizedInt = Gen.Sized(s => Gen.Choose(0, s));
            //[/sizedInt]

            //[register]
            Arb.Register<MyGenerators>();
            //[/register]
        }

    }
}
