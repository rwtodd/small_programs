using System;

namespace SnakeGame
{
    public class Program
    {
        private Snake snake;
        private bool grow;
        private ConsoleColor orig;
        private int Speed = 70;

        private void ResetConsole() {
            Console.CursorVisible = true;
            Console.ForegroundColor = orig;
        }

        public Program() {
            snake = new Snake(new Location() { X = 10, Y = 10 });
            grow = false;
        }

        public void ReadUserInput() {
            if(Console.KeyAvailable) {
                switch(Console.ReadKey().Key) {
                case ConsoleKey.UpArrow:
                    snake.ChangeDirection(0,-1);
                    break;
                case ConsoleKey.DownArrow:
                    snake.ChangeDirection(0,1);
                    break;
                case ConsoleKey.LeftArrow:
                    snake.ChangeDirection(-1,0);
                    break;
                case ConsoleKey.RightArrow:
                    snake.ChangeDirection(1,0);
                    break;
                case ConsoleKey.G:
                    grow = true;
                    break;
                case ConsoleKey.S:
                    Speed += 10;
                    break;
                case ConsoleKey.F:
                    Speed -= 10;
                    if(Speed < 10) { Speed = 10; }
                    break;
                case ConsoleKey.Q:
                    ResetConsole();
                    Environment.Exit(0);
                    break;
                }
            }
        }

        public void DrawUpdates(MovementResult mr) {
            Console.SetCursorPosition(mr.NewHead.X, mr.NewHead.Y);
            Console.ForegroundColor = ConsoleColor.Red;
            Console.Write('@');
            Console.ForegroundColor = ConsoleColor.Cyan;
            Console.SetCursorPosition(mr.OldHead.X, mr.OldHead.Y);
            Console.Write('#');
            if(mr.MovedTail.X != 0) {
                Console.SetCursorPosition(mr.MovedTail.X, mr.MovedTail.Y);
                Console.Write(' ');
            }
            Console.SetCursorPosition(Console.WindowLeft, Console.WindowTop+Console.WindowHeight);
        }

        public void RunGame() {
            Console.Clear();
            Console.CursorVisible = false;
            orig = Console.ForegroundColor;

            // write the initial location
            Console.SetCursorPosition(10,10);
            Console.Write('@');

            var mr = new MovementResult();

            while(!snake.SelfCollision()) {
                System.Threading.Thread.Sleep(Speed);
                ReadUserInput();
                snake.Move(mr, grow);
                grow = false;
                DrawUpdates(mr);
            }

        }

        public static void Main(string[] args)
        {
            var game = new Program();
            game.RunGame();
            game.ResetConsole();
        }
    }
}
