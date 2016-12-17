using System;
using System.Linq;
using System.Text;

namespace CastHex
{

  // A class to generate I Ching castings
  class CastingMethods {
	private Random rnd = new Random();
	private StringBuilder sb = new StringBuilder(6);

	// cast via the yarrow stalks method
	public string CastStalks() {
	  sb.Clear();
	  for(int i = 0; i < 6; i++) {
		var p = rnd.Next(16);
		switch(p&1) {
		case 0: sb.Append( p == 0 ? '6' : '8' ); break;
		case 1: sb.Append( p <= 5 ? '9' : '7' ); break;
		}		
	  }
	  return sb.ToString();
	}

	// cast via the 3-coins method
	public string CastCoins() {
	  sb.Clear();
	  for(int i = 0; i < 6; i++) {
		sb.Append((char)('6' + rnd.Next(2) + rnd.Next(2) + rnd.Next(2)));
	  }
	  return sb.ToString();
	}

	// cast a random hexagram with no moving lines
 	public string CastRandom() {
	  sb.Clear();
	  for(int i = 0; i < 6; i++) {
		sb.Append((char)('7' + rnd.Next(2)));
	  }
	  return sb.ToString();
	}

  }
}
