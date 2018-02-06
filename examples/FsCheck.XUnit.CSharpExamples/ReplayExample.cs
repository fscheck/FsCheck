using FsCheck.Xunit;

namespace FsCheck.XUnit.CSharpExamples
{
    public class ReplayExample
    {
        // Falsifiable, after 94 tests(2 shrinks) (17234047130667642678,3215559178322092721)
        // Last step was invoked with size of 94 and seed of(8436023154383110308,3156407541371296191):
        // Original:
        // 86
        // Shrunk:
        // 80
        [Property(Replay = "17234047130667642678,3215559178322092721,1")]
        public bool ShouldFail_0(int i)
        {
            return i < 80;
        }

        // Falsifiable, after 1 tests(2 shrinks) (8436023154383110308,3156407541371296191)
        // Last step was invoked with size of 94 and seed of(8436023154383110308,3156407541371296191):
        // Original:
        // 86
        // Shrunk:
        // 80
        [Property(Replay = "8436023154383110308,3156407541371296191,94")]
        public bool ShouldFail_1(int i)
        {
            return i < 80;
        }
    }
}