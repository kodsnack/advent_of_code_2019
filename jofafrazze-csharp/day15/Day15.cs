using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

using AdventOfCode;
using Position = AdventOfCode.GenericPosition2D<int>;

namespace day15
{
    public class Day15
    {
        readonly static string nsname = typeof(Day15).Namespace;

        static readonly Position goUp = new Position(0, -1);
        static readonly Position goRight = new Position(1, 0);
        static readonly Position goDown = new Position(0, 1);
        static readonly Position goLeft = new Position(-1, 0);
        static readonly List<Position> directions = new List<Position>()
        {
            goUp, goDown, goLeft, goRight,
        };

        class PosComputer
        {
            public IntComputer ic;
            public Position curPos;
            public Position nextPos;
            public PosComputer(IntComputer i)
            {
                ic = i;
                curPos = new Position(0, 0);
                nextPos = new Position(0, 0);
            }
            public PosComputer(PosComputer pc)
            {
                ic = new IntComputer(pc.ic);
                curPos = pc.curPos;
                nextPos = pc.nextPos;
            }
        }

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
                    { 3, new OpCode(2, /* inp */ delegate() { Write(Addr(1), reg); return 1; }) },
                    { 4, new OpCode(2, /* out */ delegate() { reg = Read(Addr(1)); return 2; }) },
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

            public bool Execute()
            {
                instructionsExecuted = 0;
                while (pc >= 0)
                {
                    int opc = (int)mem[pc] % 100;
                    if (opc == 99)
                        return false;
                    instructionsExecuted++;
                    int ret = instructionSet[opc].func();
                    if (ret > 0)
                        pc += instructionSet[opc].length;
                    if (ret == 2)
                        return true;
                }
                return false;
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
                    char c = 'X';
                    switch (kvp.Value)
                    {
                        case 0: c = '#'; break;
                        case 1: c = ' '; break;
                        case 2: c = 'O'; break;
                    }
                    map.data[p.x - x0, p.y - y0] = c;
                }
                if (x0 <= 0 && x1 >= 0 && y0 <= 0 && y1 >= 0)
                    map.data[0 - x0, 0 - y0] = 'S';
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

        static void SetupComputerMovement(ref PosComputer c, int dirIndex)
        {
            c.ic.reg = dirIndex + 1;
            c.nextPos = c.curPos + directions[dirIndex];
        }

        static OurMap BuildMap(PosComputer c1, bool print)
        {
            OurMap m = new OurMap();
            List<PosComputer> computers = new List<PosComputer>();
            SetupComputerMovement(ref c1, 0);
            computers.Add(c1);
            do
            {
                List<PosComputer> newComputers = new List<PosComputer>();
                foreach (PosComputer c in computers)
                {
                    if (c.ic.Execute())
                    {
                        // 0 = Hit wall, 1 = Has moved, 2 = Has moved, found treasure
                        int res = (int)c.ic.reg;
                        m.mapPos[c.nextPos] = res;
                        if (res > 0)
                            c.curPos = c.nextPos;
                        // Movement command values: 1 = N, 2 = S, 3 = W, 4 = E
                        for (int i = 0; i < directions.Count; i++)
                        {
                            Position newPos = c.curPos + directions[i];
                            if (!m.mapPos.ContainsKey(newPos))
                            {
                                PosComputer x = new PosComputer(c);
                                SetupComputerMovement(ref x, i);
                                newComputers.Add(x);
                            }
                        }
                    }
                }
                computers = newComputers;
                if (print)
                {
                    Console.SetCursorPosition(0, 1);
                    m.PrintMap();
                }
            }
            while (computers.Count > 0);
            return m;
        }

        static int CalculateStepsToTreasure(OurMap m)
        {
            Dictionary<Position, int> steps = new Dictionary<Position, int>();
            List<Position> moveFrom = new List<Position>();
            Position p1 = new Position(0, 0);
            moveFrom.Add(p1);
            steps[p1] = 0;
            int minSteps = int.MaxValue;
            do
            {
                List<Position> nextMoveFrom = new List<Position>();
                foreach (Position p in moveFrom)
                {
                    if (m.mapPos[p] == 2)
                    {
                        if (steps[p] < minSteps)
                            minSteps = steps[p];
                    }
                    for (int i = 0; i < directions.Count; i++)
                    {
                        Position newPos = p + directions[i];
                        if (m.mapPos[newPos] > 0)
                        {
                            int newSteps = steps[p] + 1;
                            if (!steps.ContainsKey(newPos) || steps[newPos] > newSteps)
                            {
                                nextMoveFrom.Add(newPos);
                                steps[newPos] = newSteps;
                            }
                        }
                    }
                }
                moveFrom = nextMoveFrom;
            }
            while (moveFrom.Count > 0);
            return minSteps;
        }

        static int CalculateOxygenFillMinutes(OurMap m)
        {
            Dictionary<Position, int> steps = new Dictionary<Position, int>();
            List<Position> moveFrom = new List<Position>();
            Position p1 = m.mapPos.Where(x => x.Value == 2).First().Key;
            moveFrom.Add(p1);
            steps[p1] = 0;
            do
            {
                List<Position> nextMoveFrom = new List<Position>();
                foreach (Position p in moveFrom)
                {
                    for (int i = 0; i < directions.Count; i++)
                    {
                        Position newPos = p + directions[i];
                        if (m.mapPos[newPos] > 0)
                        {
                            int newSteps = steps[p] + 1;
                            if (!steps.ContainsKey(newPos) || steps[newPos] > newSteps)
                            {
                                nextMoveFrom.Add(newPos);
                                steps[newPos] = newSteps;
                            }
                        }
                    }
                }
                moveFrom = nextMoveFrom;
            }
            while (moveFrom.Count > 0);
            return steps.Max(x => x.Value);
        }

        static bool PartA(Object correctAnswer = null)
        {
            List<long> input = ReadInput();
            IntComputer c1 = new IntComputer(input, 0);
            PosComputer p1 = new PosComputer(c1);
            OurMap m = BuildMap(p1, true);
            Console.SetCursorPosition(0, 1);
            m.PrintMap();
            int ans = CalculateStepsToTreasure(m);
            Console.WriteLine("Part A: Result is {0}", ans);
            return correctAnswer == null || ans == (int)correctAnswer;
        }

        static bool PartB(Object correctAnswer = null)
        {
            List<long> input = ReadInput();
            IntComputer c1 = new IntComputer(input, 0);
            PosComputer p1 = new PosComputer(c1);
            OurMap m = BuildMap(p1, false);
            int b = CalculateOxygenFillMinutes(m);
            Console.WriteLine("Part B: Result is {0}", b);
            return correctAnswer == null || b == (int)correctAnswer;
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2019 - " + nsname + ":");
            PartA();
            PartB();
        }

        public static bool MainTest()
        {
            int a = 308;
            int b = 328;
            return PartA(a) && PartB(b);
        }
    }
}
