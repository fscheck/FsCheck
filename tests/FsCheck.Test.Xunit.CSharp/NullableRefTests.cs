using FsCheck;
using FsCheck.Xunit;

public class NullableRefTests
{
    [Property(MaxTest = 100)]
    public bool GeneratesNullOrString(NullableRef<string> nullableRef) =>
        nullableRef.Value is null || nullableRef.Value is string;
}
