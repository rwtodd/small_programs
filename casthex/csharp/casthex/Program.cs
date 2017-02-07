using System;
using System.Linq;

namespace CastHex
{
    public class Program
    {

        public static void Main(string[] args)
        {
            var arg = (args.Length > 0) ? args[0] : "-coins";
            var methods = new CastingMethods();

            switch (arg)
            {
                case "-coins": arg = methods.CastCoins(); break;
                case "-stalks": arg = methods.CastStalks(); break;
                case "-random": arg = methods.CastRandom(); break;
            }

            if ((arg.Length != 6) || arg.Any(ch => ch < '6' || ch > '9'))
            {
                Console.Error.WriteLine(@"Usage: casthex [-opt|<casting>]
 Options:
  -coins    use 3-coins method
  -stalks   use yarrow stalks method
  -random   generate a random hexagram
  <casting> a 6-digit string of characters in the set {6,7,8,9}");
                Environment.Exit(1);
            }

			new Formatter().Format(System.Console.Out, arg);
        }
    }
}
