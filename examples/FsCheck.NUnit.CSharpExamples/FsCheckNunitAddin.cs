using FsCheck.NUnit.Addin;
using NUnit.Core.Extensibility;

namespace FsCheck.NUnit.CSharpExamples
{
    [NUnitAddin(Description = "FsCheck addin")]
    public class FsCheckNunitAddin : IAddin
    {
        public bool Install(IExtensionHost host)
        {
            var tcBuilder = new FsCheckTestCaseBuilder();
            host.GetExtensionPoint("TestCaseBuilders").Install(tcBuilder);
            return true;
        }
    }
}