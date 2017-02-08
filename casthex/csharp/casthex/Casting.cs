using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;
using System.Text;

namespace CastHex
{
    public partial class Casting
    {
        private String casting;

        /// <summary>
        /// Create a casting from a provided function.
        /// </summary>
        public Casting(Func<Char> method) {
            var sb = new StringBuilder(6);
            for(int i = 0; i < 6; i++) sb.Append(method());
            casting = sb.ToString();
        }

        /// <summary>
        /// Create a casting from the '6','7','8','9' string representation.
        /// </summary>
        public Casting(String c) {
            casting = c;
        }

        /// <summary>
        /// Output a description of the casting on the given TextWriter.
        /// </summary>
        public void Format(TextWriter tw) {
            var reps = new List<String>(6);
            var wen1 = 0;
            var wen2 = 0;
            foreach (var ch in casting.Reverse())
            {
                wen1 *= 2;
                wen2 *= 2;
                switch(ch) {
                    case '6': wen2 += 1;            reps.Add("  ---   ---  ->  ---------"); break;
                    case '7': wen1 += 1; wen2 += 1; reps.Add("  ---------      ---------"); break;
                    case '8':                       reps.Add("  ---   ---      ---   ---"); break; 
                    case '9': wen1 += 1;            reps.Add("  ---------  ->  ---   ---"); break; 
                }
            }

            var changed = wen1 != wen2;
            tw.WriteLine("Casting <{0}>\n", casting);
            reps.ForEach( s => tw.WriteLine( changed ? s : s.Substring(0, 11)) );
            tw.WriteLine();
            tw.WriteLine(hexName[wen1]);
            if(changed) {
                tw.WriteLine(" - Changing To ->\n{0}", hexName[wen2]);
            }
        }
    }
}