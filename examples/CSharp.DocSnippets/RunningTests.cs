using FsCheck;

namespace CSharp.DocSnippets
{
    class RunningTests
    {
        public static void Samples()
        {
            //[configuration]
            var configuration = Configuration.Quick;
            configuration.MaxNbOfTest = 1000;
            configuration.QuietOnSuccess = true;
            true.ToProperty().Check(configuration);
            //[/configuration]
        }
    }
}


