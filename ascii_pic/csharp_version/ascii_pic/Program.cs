using System;
using System.IO;
using System.Text;
using ImageSharp;
using Microsoft.Extensions.CommandLineUtils;

namespace ConsoleApplication
{
    public class Program
    {
        private static double Brightness(Color c) => c.R * 0.2126 + c.G * 0.7152 + c.B * 0.0722;

        private static char[] translation = new char[] { '#', 'A', '@', '%', '$', '+', '=', '*', ':', ',', '.', ' ' };

        private static char ToChar(double b) => translation[(int)(b * translation.Length / 256.0)];

        public static void Main(string[] args)
        {
            var cmd = new CommandLineApplication(throwOnUnexpectedArg: false);
            cmd.HelpOption("-h |--help |-? ");
            var widthOption = cmd.Option("-w|--width", 
                "the desired width of the image (default: 72)", 
                CommandOptionType.SingleValue);
            var arOption = cmd.Option("-ar|--aspectratio",
                "the ratio of char height to width (default: 2.0)", 
                CommandOptionType.SingleValue);
            var wobOption = cmd.Option("-wob|--white-on-black",
                "reverse video for light text on dark backgrounds",
                CommandOptionType.NoValue);
            cmd.OnExecute(() =>
            {   
                if (cmd.RemainingArguments.Count != 1) return 1;
                var opWide = widthOption.HasValue()? Int32.Parse(widthOption.Value()) : 72;
                var opAr   = arOption.HasValue()? Double.Parse(arOption.Value()) : 2.0 ;
                if(wobOption.HasValue()) { Array.Reverse(translation); }

                using (FileStream stream = File.OpenRead(cmd.RemainingArguments[0]))
                {
                    Image image = new Image(stream);
                    var sb = new StringBuilder();
                    var ht = (int)(opWide / opAr / image.Width * image.Height); 
                    using (var pa = image.Resize(opWide, ht).Lock())
                    {
                        for (int y = 0; y < pa.Height; y++)
                        {
                            for (int x = 0; x < pa.Width; x++)
                            {
                                sb.Append(ToChar(Brightness(pa[x, y])));
                            }
                            sb.AppendLine();
                        }
                    }
                    Console.WriteLine(sb.ToString());
                }

                return 0;
            });

            var ecode = cmd.Execute(args);
            if(ecode != 0) Environment.Exit(ecode);
        }
    }
}
