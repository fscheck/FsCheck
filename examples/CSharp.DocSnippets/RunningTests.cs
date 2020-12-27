using FsCheck;

namespace CSharp.DocSnippets
{
    class RunningTests
    {
        public static void Samples()
        {
            //[configuration]
            var configuration = Config.Quick
                                      .WithMaxTest(1000)
                                      .WithQuietOnSuccess(true);
            true.ToProperty().Check(configuration);
            //[/configuration]
        }
    }
}


