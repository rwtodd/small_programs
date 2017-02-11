using System;

namespace SnakeGame {
    public struct Location { public byte X; public byte Y; }
    public class MovementResult {
        public Location NewHead;
        public Location OldHead;
        public Location MovedTail;
    }

    public class Snake {
        private Location[] Segments;
        private int HeadIdx;
        private int deltaX;
        private int deltaY;

        public Snake(Location init) {
            Segments = new Location[] { init };
            HeadIdx = 0;
            deltaX = 1;
            deltaY = 0;
        }
        
        public void Move(MovementResult mr, bool grow) {
            mr.OldHead = Segments[HeadIdx];
            if(grow) {
                Array.Resize(ref Segments, Segments.Length+1);
                HeadIdx = HeadIdx + 1;
                if(HeadIdx < (Segments.Length -1)) {
                    Array.Copy(Segments, HeadIdx, Segments, HeadIdx+1, Segments.Length - HeadIdx - 1);
                }
                mr.MovedTail.X = 0;
                mr.MovedTail.Y = 0;
            } else {
                HeadIdx = HeadIdx + 1;
                if(HeadIdx == Segments.Length) {
                    HeadIdx = 0;
                }
                mr.MovedTail = Segments[HeadIdx];
            }
            // update the head and set it...
            Segments[HeadIdx].X = (byte)(mr.OldHead.X + deltaX);
            Segments[HeadIdx].Y = (byte)(mr.OldHead.Y + deltaY);
            mr.NewHead = Segments[HeadIdx];
        }

        public void ChangeDirection(int dx, int dy) {
            deltaX = dx;
            deltaY = dy;
        }

        public bool Collision(Location target, bool skiphead=false) {
            bool found = false;
            for(var i = 0; i < Segments.Length; i++) {
                if( (i == HeadIdx) && skiphead ) continue;
                if((Segments[i].X == target.X) &&
                   (Segments[i].Y == target.Y)) {
                       found = true;
                       break;
                   }
            }
            return found;
        }
        public bool SelfCollision() => Collision(Segments[HeadIdx], true);
    }
    
}