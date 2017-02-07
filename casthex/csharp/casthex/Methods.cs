using System;
using System.Text;

namespace CastHex
{

    // A class to generate I Ching castings
    class CastingMethods
    {
        private Random rnd = new Random();
        private StringBuilder sb = new StringBuilder(6);

        private String cast(Func<Char> fn) {
            sb.Clear();
            for(int i = 0; i < 6; i++) {
                sb.Append(fn());
            }
            return sb.ToString();
        }

        // cast via the yarrow stalks method
        public string CastStalks() =>
           cast( () => {
                var p = rnd.Next(16);
                char ans = '6';
                switch (p & 1)
                {
                    case 0: ans = (p == 0 ? '6' : '8'); break;
                    case 1: ans = (p <= 5 ? '9' : '7'); break;
                }
                return ans;
            });
        

        // cast via the 3-coins method
        public string CastCoins() => cast( () => (char)('6' + rnd.Next(2) + rnd.Next(2) + rnd.Next(2)));
        
        // cast a random hex with no moving lines
        public string CastRandom() => cast( () => (char)('7' + rnd.Next(2) ));

    }
}
