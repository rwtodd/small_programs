using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Numerics;

namespace AsciiMandelbrot
{
    class Program
    {
        private char[] buffer;
        private double ulX;
        private double ulY;
        private double scaleX;
        private double scaleY;

        private char iterate(Complex pt)
        {
            Complex cur = pt;
            char ans = '~';
            while (ans > ' ')
            {
                if ((cur.Real * cur.Real + cur.Imaginary * cur.Imaginary) > 4.0) break;
                cur = cur * cur + pt;
                --ans;
            }
            return ans;
        }

        private void scene() {
            Console.SetCursorPosition(0,0);
            var wh = Console.WindowHeight - 1;
            for(int idy = 0; idy < wh; ++idy) {
                var curY = ulY + idy * scaleY;
                for (int idx = 0; idx < buffer.Length; ++idx)
                {
                    buffer[idx] = iterate(new Complex(ulX + idx * scaleX, curY));
                }
                Console.Write(buffer);
            }
            Console.Out.Flush();
        }

        public Program()
        {
            buffer = new char[Console.WindowWidth];
            ulX = -2.0; ulY = -1.0;
            scaleX = 0.04; scaleY = 0.1;
        }

        public void MainLoop()
        {
            var wh = Console.WindowHeight - 1;
            while (true) 
            {
                scene();
                var k = Console.ReadKey(true);
                switch(k.Key) {
                    case ConsoleKey.UpArrow:
                        ulY -= 3.0 * scaleY;   break;
                    case ConsoleKey.DownArrow:
                        ulY += 3.0 * scaleY;   break;
                    case ConsoleKey.LeftArrow:
                        ulX -= 6.0 * scaleX;   break;
                    case ConsoleKey.RightArrow:
                        ulX += 6.0 * scaleX;   break;
                    case ConsoleKey.I:
                        ulX += scaleX * buffer.Length / 4;
                        ulY += scaleY * wh / 4;
                        scaleX /= 2;
                        scaleY /= 2;
                        break;
                    case ConsoleKey.O:
                        scaleX *= 2;
                        scaleY *= 2;
                        ulX -= scaleX * buffer.Length / 4;
                        ulY -= scaleY * wh / 4;
                        break;
                    case ConsoleKey.Q:
                        return;
                }
            }

        }

        static void Main(string[] args)
        {
            try
            {
                Console.Clear();
                Console.CursorVisible = false;
                var pgm = new Program();
                pgm.MainLoop();
            }
            finally
            {
                Console.Clear();
                Console.CursorVisible = true;
            }
        }
    }
}
