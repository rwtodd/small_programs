using System;
using System.IO;
using System.Text;
using ImageSharp;

namespace ConsoleApplication
{
    public class Program
    {
        private static double Brightness(Color c) =>  c.R * 0.2126 + c.G * 0.7152 + c.B * 0.0722;

        private static char[] translation = new char[] { '#','A','@','%','$','+','=','*',':',',','.',' ' };

        private static char ToChar(Color c) => translation[(int)(Brightness(c)*translation.Length/256.0)];

        public static void Main(string[] args)
        {
            using (FileStream stream = File.OpenRead(args[0]))
            {
                Image image = new Image(stream);
                var sb = new StringBuilder();
                using(var pa = image.Resize(72, 0).Lock()) {
                    for(int y = 0; y < pa.Height; y++) {
                        for(int x = 0; x < pa.Width; x++) {
                            sb.Append(ToChar(pa[x,y]));
                        }
                        sb.AppendLine();
                    }
                }
                Console.WriteLine(sb.ToString());
            }
        }
    }
}
