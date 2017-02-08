using System;

namespace CastHex
{

    // A class to generate I Ching castings
    class CastingMethods
    {
        private Random rnd = new Random();

        // cast via the yarrow stalks method
        public Char CastStalks() {
                var p = rnd.Next(16);
                char ans = '6';
                switch (p & 1)
                {
                    case 0: ans = (p == 0 ? '6' : '8'); break;
                    case 1: ans = (p <= 5 ? '9' : '7'); break;
                }
                return ans;
        }
        
        // cast via the 3-coins method
        public Char CastCoins() =>  (char)('6' + rnd.Next(2) + rnd.Next(2) + rnd.Next(2));
        
        // cast a random hex with no moving lines
        public Char CastRandom() => (char)('7' + rnd.Next(2) );

    }
}
