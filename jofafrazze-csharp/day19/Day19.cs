using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

using AdventOfCode;
using Position = AdventOfCode.GenericPosition2D<int>;

namespace day19
{
    public class Day19
    {
        readonly static string nsname = typeof(Day19).Namespace;

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
                    char c = kvp.Value == 1 ? '#' : '.';
                    
                    map.data[p.x - x0, p.y - y0] = c;
                }
                map.Print();
            }
        }

        static List<long> ReadInput()
        {
            string path = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");
            StreamReader reader = File.OpenText(path);
            List<long> list = new List<long>();
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                list.AddRange(line.Split(',').Select(long.Parse).ToList());
            }
            return list;
        }

        static OurMap BuildMap(IntComputer ic, int size)
        {
            OurMap m = new OurMap();
            Position curPos = new Position(0, 0);
            for (int y = 0; y < size; y++)
            {
                for (int x = 0; x < size; x++)
                {
                    IntComputer compu = new IntComputer(ic);
                    compu.reg = x;
                    compu.Execute();
                    compu.reg = y;
                    compu.Execute();
                    compu.Execute();
                    int a = (int)compu.reg;
                    m.mapPos[new Position(x, y)] = a;
                }
            }
            return m;
        }

        static int ReadPosition(IntComputer ic, Position pos)
        {
            IntComputer compu = new IntComputer(ic);
            compu.reg = pos.x;
            compu.Execute();
            compu.reg = pos.y;
            compu.Execute();
            compu.Execute();
            return (int)compu.reg;
        }

        static bool SquareFits(IntComputer ic, int xStart, int yMax, ref Position pos)
        {
            Position dw = new Position(99, 0);
            Position dh = new Position(0, 99);
            Position blPos = new Position(xStart, yMax);
            while (ReadPosition(ic, blPos) != 1)
                blPos += goRight;
            Position brPos = blPos + dw;
            Position tlPos = blPos - dh;
            Position trPos = tlPos + dw;
            List<Position> positions = new List<Position>() { brPos, tlPos, trPos };
            bool allFit = true;
            foreach (Position p in positions)
                if (ReadPosition(ic, p) != 1)
                    allFit = false;
            if (allFit)
                pos = tlPos;
            return allFit;
        }

        static Position CalculatePosition(IntComputer ic, int dx, int dy)
        {
            Position pos = new Position(0, 0);
            int x = dx;
            int y = dy;
            while (!SquareFits(ic, x, y, ref pos))
            {
                x += dx;
                y += dy;
            }
            x = pos.x - 1;
            do
            {
                x--;
                y--;
            }
            while (SquareFits(ic, x, y, ref pos));
            return pos;
        }

        static Object PartA()
        {
            List<long> input = ReadInput();
            IntComputer c0 = new IntComputer(input, 0);
            OurMap m = BuildMap(c0, 50);
            m.PrintMap();
            int a = m.mapPos.Sum(x => x.Value);
            Console.WriteLine("Part A: Result is {0}", a);
            return a;
        }

        static Object PartB()
        {
            List<long> input = ReadInput();
            IntComputer c0 = new IntComputer(input, 0);
            OurMap m = BuildMap(c0, 50);
            int dy = 49;
            int dx = m.mapPos.Where(g => g.Key.y == dy && g.Value == 1).Min(g => g.Key.x);
            Position p = CalculatePosition(c0, dx - 1, dy);
            int b = p.y + p.x * 10000;
            Console.WriteLine("Part B: Result is {0}", b);
            return b;
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2019 - " + nsname + ":");
            PartA();
            PartB();
        }

        public static bool MainTest()
        {
            int a = 181;
            int b = 4240964;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
