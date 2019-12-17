using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;

using AdventOfCode;
using Position = AdventOfCode.GenericPosition2D<int>;

namespace day17
{
    class Day17
    {
        static readonly Position goUp = new Position(0, -1);
        static readonly Position goRight = new Position(1, 0);
        static readonly Position goDown = new Position(0, 1);
        static readonly Position goLeft = new Position(-1, 0);
        static readonly List<Position> directions = new List<Position>()
        {
            goUp, goRight, goDown, goLeft, 
        };

        public class IntComputer
        {
            struct OpCode
            {
                public int length;
                public Func<int> func;
                public OpCode(int l, Func<int> f) { length = l; func = f; }
            };
            public Dictionary<long, long> mem;
            public long reg;
            public long rbase;
            public long pc;
            private Dictionary<int, OpCode> instructionSet;
            public int instructionsExecuted;
            public IntComputer(IntComputer c)
            {
                mem = new Dictionary<long, long>(c.mem);
                reg = c.reg;
                rbase = c.rbase;
                pc = c.pc;
                InitInstructionSet();
                instructionsExecuted = c.instructionsExecuted;
            }
            public IntComputer(List<long> mem0, long reg0)
            {
                mem = mem0.Select((v, i) => new { v, i }).ToDictionary(x => (long)x.i, x => x.v);
                reg = reg0;
                rbase = 0;
                pc = 0;
                InitInstructionSet();
                instructionsExecuted = 0;
            }
            private void InitInstructionSet()
            {
                instructionSet = new Dictionary<int, OpCode>()
                {
                    { 1, new OpCode(4, /* add */ delegate() { Write(Addr(3), Read(Addr(1)) + Read(Addr(2))); return 1; }) },
                    { 2, new OpCode(4, /* mul */ delegate() { Write(Addr(3), Read(Addr(1)) * Read(Addr(2))); return 1; }) },
                    { 3, new OpCode(2, /* inp */ delegate() { Write(Addr(1), reg); return 2; }) },
                    { 4, new OpCode(2, /* out */ delegate() { reg = Read(Addr(1)); return 3; }) },
                    { 5, new OpCode(3, /* jit */ delegate() { bool j = Read(Addr(1)) != 0; if (j) pc = Read(Addr(2)); return j ? 0 : 1; }) },
                    { 6, new OpCode(3, /* jif */ delegate() { bool j = Read(Addr(1)) == 0; if (j) pc = Read(Addr(2)); return j ? 0 : 1; }) },
                    { 7, new OpCode(4, /* lth */ delegate() { Write(Addr(3), Read(Addr(1)) < Read(Addr(2)) ? 1 : 0); return 1; }) },
                    { 8, new OpCode(4, /* equ */ delegate() { Write(Addr(3), Read(Addr(1)) == Read(Addr(2)) ? 1 : 0); return 1; }) },
                    { 9, new OpCode(2, /* adb */ delegate() { rbase += Read(Addr(1)); return 1; }) },
                };
            }

            private long Read(long addr) { return mem.ContainsKey(addr) ? mem[addr] : 0; }
            private void Write(long addr, long value) { mem[addr] = value; }
            private int GetAdressingMode(int offs)
            {
                int v = (int)Read(pc) / 100;
                for (int i = 1; i < offs; i++)
                    v /= 10;
                return v % 10;
            }
            private long Addr(int offs)
            {
                switch (GetAdressingMode(offs))
                {
                    case 0: return Read(pc + offs);
                    case 1: return pc + offs;
                    case 2: return rbase + Read(pc + offs);
                }
                throw new ArgumentOutOfRangeException();
            }

            public int Execute()
            {
                instructionsExecuted = 0;
                while (pc >= 0)
                {
                    int opc = (int)mem[pc] % 100;
                    if (opc == 99)
                        return 0;
                    instructionsExecuted++;
                    int ret = instructionSet[opc].func();
                    if (ret > 0)
                        pc += instructionSet[opc].length;
                    if (ret >= 2)
                        return ret;
                }
                return 0;
            }
        }

        public class OurMap
        {
            public Dictionary<Position, int> mapPos;
            public OurMap()
            {
                mapPos = new Dictionary<Position, int>();
            }

            public void PrintMap()
            {
                if (mapPos.Count == 0)
                    return;
                int x0 = mapPos.Min(a => a.Key.x);
                int x1 = mapPos.Max(a => a.Key.x);
                int y0 = mapPos.Min(a => a.Key.y);
                int y1 = mapPos.Max(a => a.Key.y);
                int w = x1 - x0 + 1;
                int h = y1 - y0 + 1;
                Map map = new Map(w, h, new Position(0, 0), '?');
                foreach (var kvp in mapPos)
                {
                    Position p = kvp.Key;
                    char c = (char)kvp.Value;
                    map.data[p.x - x0, p.y - y0] = c;
                }
                map.Print();
            }
        }

        static List<long> ReadInput()
        {
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\..\input.txt");
            StreamReader reader = File.OpenText(path);
            List<long> list = new List<long>();
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                list.AddRange(line.Split(',').Select(long.Parse).ToList());
            }
            return list;
        }

        static OurMap BuildMap(IntComputer ic)
        {
            OurMap m = new OurMap();
            Position curPos = new Position(0, 0);
            while (ic.Execute() > 0)
            {
                int a = (int)ic.reg;
                if (a == '\n')
                {
                    curPos.x = 0;
                    curPos.y++;
                }
                else
                {
                    m.mapPos[curPos] = a;
                    curPos += goRight;
                }
            }
            return m;
        }

        static bool MapPositionHasValue(OurMap m, Position pos, int value)
        {
            return m.mapPos.ContainsKey(pos) && m.mapPos[pos] == value;
        }

        static Position FindDirection(OurMap m, Position pos, int value)
        {
            foreach (Position p in directions)
            {
                Position test = pos + p;
                if (MapPositionHasValue(m, test, value))
                    return p;
            }
            throw new ArgumentOutOfRangeException();
        }

        static int CalculateAlignmentParametersSum(OurMap m)
        {
            Position curPos = m.mapPos.Where(x => (x.Value != '.') && (x.Value != '#')).First().Key;
            Position dir = FindDirection(m, curPos, '#');
            HashSet<Position> visited = new HashSet<Position>();
            List<Position> intersections = new List<Position>();
            bool done = false;
            while (!done)
            {
                if (visited.Contains(curPos))
                    intersections.Add(curPos);
                visited.Add(curPos);
                List<Position> nextMoves = new List<Position>() { dir };
                int dirIndex = directions.IndexOf(dir);
                nextMoves.Add(directions[(dirIndex - 1 + 4) % 4]);
                nextMoves.Add(directions[(dirIndex + 1) % 4]);
                done = true;
                for (int i = 0; i < nextMoves.Count && done; i++)
                {
                    if (MapPositionHasValue(m, curPos + nextMoves[i], '#'))
                    {
                        dir = nextMoves[i];
                        done = false;
                    }
                }
                curPos += dir;
            }
            return intersections.Select(a => a.x * a.y).Sum();
        }

        static readonly List<char> initialDirections = new List<char> { '^', '>', 'v', '<' };

        static void BuildMovementSequence(OurMap m)
        {
            Position curPos = m.mapPos.Where(x => (x.Value != '.') && (x.Value != '#')).First().Key;
            int idx = initialDirections.IndexOf((char)m.mapPos[curPos]);
            Position dir = directions[idx];
            string mf = "";
            bool done = false;
            int nMoves = 0;
            while (!done)
            {
                List<Position> nextMoves = new List<Position>() { dir };
                int dirIndex = directions.IndexOf(dir);
                nextMoves.Add(directions[(dirIndex - 1 + 4) % 4]);
                nextMoves.Add(directions[(dirIndex + 1) % 4]);
                done = true;
                for (int i = 0; i < nextMoves.Count && done; i++)
                {
                    if (MapPositionHasValue(m, curPos + nextMoves[i], '#'))
                    {
                        if (i == 0)
                        {
                            nMoves++;
                        }
                        else
                        {
                            if (nMoves > 0)
                            {
                                nMoves++;
                                mf += nMoves.ToString() + ',';
                                nMoves = 0;
                            }
                            string s = (i == 1) ? "L" : "R";
                            mf += s + ',';
                        }
                        dir = nextMoves[i];
                        done = false;
                    }
                }
                curPos += dir;
            }
            Console.WriteLine(mf);
        }

        static int SteerRobot(IntComputer ic)
        {
            string A = "L,6,R,12,R,8";
            string B = "R,8,R,12,L,12";
            string C = "R,12,L,12,L,4,L,4";
            string M = "A,B,B,A,C,A,C,A,C,B";
            List<char> feed = new List<char>();
            feed.AddRange(M.ToList());
            feed.Add('\n');
            feed.AddRange(A.ToList());
            feed.Add('\n');
            feed.AddRange(B.ToList());
            feed.Add('\n');
            feed.AddRange(C.ToList());
            feed.Add('\n');
            feed.Add('n');
            feed.Add('\n');
            int ret = 0;
            int i = 0;
            int lastReg = 0;
            do
            {
                lastReg = (int)ic.reg;
                ic.reg = feed[Math.Min(i, feed.Count - 1)];
                ret = ic.Execute();
                if (ret == 2)
                {
                    i++;
                }
                else if (ret == 3)
                {
                    //Console.Write((char)ic.reg);
                }
            }
            while (ret > 0);
            return lastReg;
        }

        static void PartA()
        {
            List<long> input = ReadInput();
            IntComputer c1 = new IntComputer(input, 0);
            OurMap m = BuildMap(c1);
            Console.SetCursorPosition(0, 1);
            m.PrintMap();
            int ans = CalculateAlignmentParametersSum(m);
            Console.WriteLine("Part A: Result is {0}", ans);
            //BuildMovementSequence(m);
        }

        static void PartB()
        {
            List<long> input = ReadInput();
            IntComputer c2 = new IntComputer(input, 0);
            c2.mem[0] = 2;
            int ans2 = SteerRobot(c2);
            Console.WriteLine("Part B: Result is {0}", ans2);
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2019 - " + typeof(Day17).Namespace + ":");
            PartA();
            PartB();
        }
    }
}
