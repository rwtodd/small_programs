using System;
using System.Linq;

namespace CastHex
{
    public partial class Program
    {

	private static string[] LINES = new string[] { 
              "\u2584\u2584   \u2584\u2584",
	      "\u2584\u2584\u2584\u2584\u2584\u2584\u2584"
	};

	private static void Display(String casting) {
		Func<char,bool> sixOrNine = ch => ch == '6' || ch == '9';
		bool changes = casting.Any(sixOrNine);
		Console.WriteLine("Casting: <{0}>\n", casting);
		int wen1 = 0;
		int wen2 = 0;
		var format = changes?"  {0}  {1}  {2}":"  {0}";
		foreach(char c in casting.Reverse()) {
			int idx1 = c&1;
			int idx2 = idx1;
		  	if(sixOrNine(c)) { idx2 = 1 - idx2; }
			wen1 = (wen1 << 1) | idx1;
			wen2 = (wen2 << 1) | idx2;
			Console.WriteLine(format, LINES[idx1], (idx1==idx2)?"   ":"-->", LINES[idx2]);
		}
		Console.WriteLine("\n{0}", hexName[wen1]);
		if(changes) {
			Console.WriteLine(" - Changing To -->\n{0}", hexName[wen2]);
		}
	}

        public static void Main(string[] args)
        {
		var arg = (args.Length > 0)?args[0]:"-coins";
		var methods = new CastingMethods();

		switch(arg) {
		case "-coins": arg = methods.CastCoins(); break;
		case "-stalks": arg = methods.CastStalks(); break;
		case "-random": arg = methods.CastRandom(); break;
		}

	 	if( (arg.Length != 6) || arg.Any( ch => ch < '6' || ch > '9' ) ) {
			Console.Error.WriteLine(@"Usage: casthex [-opt|<casting>]
 Options:
  -coins    use 3-coins method
  -stalks   use yarrow stalks method
  -random   generate a random hexagram
  <casting> a 6-digit string of characters in the set {6,7,8,9}");
			Environment.Exit(1);
		}

		Display(arg);
        }
    }
}
