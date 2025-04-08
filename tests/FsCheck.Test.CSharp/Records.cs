using System;

namespace FsCheck.Test.CSharp
{
    public record RgbColor(byte Red, byte Green, byte Blue) { }

    public record CsRecordExample1(int I, string S, char C) { }

    public record CsRecordExample2(CsRecordExample1 Ex1, RgbColor Rgb) { }

    public record Person
    {
        public string FirstName { get; init; }
        public string LastName { get; init; }
    }

    public record PersonWithHeight : Person
    {
        public int HeightInInches { get; init; }

        public PersonWithHeight Grow(int inches) =>
          this with { HeightInInches = HeightInInches + inches };
    }

    public record CtorAndProps(int A)
    {
        public int B { get; init; }
    }

    public readonly record struct ReadOnlyStructPositionalRecord(byte Red, byte Green, byte Blue);

    public readonly record struct ReadOnlyStructInitOnlyRecord
    {
        public byte Red { get; init; }
        public byte Green { get; init; }
        public byte Blue { get; init; }
    }

    public record struct MutableStructPositionalRecord(byte Red, byte Green, byte Blue);

    public record struct MutableStructRecord
    {
        public byte Red { get; set; }
        public byte Green { get; set; }
        public byte Blue { get; set; }
    }
}
