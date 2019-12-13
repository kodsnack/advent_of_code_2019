using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;

using AdventOfCode;
using Position = AdventOfCode.GenericPosition2D<int>;

namespace day13
{
    class Day13
    {
        public class IntComputer
        {
            struct OpCode
            {
                public int length;
                public Func<bool> func;
                public OpCode(int l, Func<bool> f) { length = l; func = f; }
            };
            public Dictionary<long, long> mem;
            public long reg;
            public long rbase;
            public long pc;
            private readonly Dictionary<int, OpCode> instructionSet;
            public int instructionsExecuted;
            public Position ballPos;
            public Position paddlePos;
            public Position curPos;
            public Dictionary<Position, int> paintedPos;
            public int nOut;
            public int curX, curY;
            public IntComputer(List<long> mem0, long reg0)
            {
                mem = mem0.Select((v, i) => new { v, i }).ToDictionary(x => (long)x.i, x => x.v);
                reg = reg0;
                rbase = 0;
                pc = 0;
                curPos = new Position(0, 0);
                ballPos = new Position(-1, -1);
                paddlePos = new Position(-1, -1);
                paintedPos = new Dictionary<Position, int>();
                nOut = 0;
                curX = 0;
                curY = 0;
                instructionSet = new Dictionary<int, OpCode>()
                {
                    { 1, new OpCode(4, /* add */ delegate() { Write(Addr(3), Read(Addr(1)) + Read(Addr(2))); return true; }) },
                    { 2, new OpCode(4, /* mul */ delegate() { Write(Addr(3), Read(Addr(1)) * Read(Addr(2))); return true; }) },
                    { 3, new OpCode(2, /* inp */ delegate() {
                        reg = 0;
                        if (ballPos.x >= 0 && paddlePos.x > 0)
                            reg = ballPos.x.CompareTo(paddlePos.x);
                        Write(Addr(1), reg);
                        return true;
                        })
                    },
                    { 4, new OpCode(2, /* out */ delegate() {
                        reg = Read(Addr(1));
                        if (nOut % 3 == 0)
                        {
                            curPos.x = (int) reg;
                        }
                        else if (nOut % 3 == 1)
                        {
                            curPos.y = (int) reg;
                        }
                        else
                        {
                            if (curPos == new Position(-1, 0))
                            {
                                Console.SetCursorPosition(0, Console.CursorTop - 1);
                                Console.WriteLine("{0}", reg);
                            }
                            else
                            {
                                if (reg == 3) // paddle
                                    paddlePos = curPos;
                                if (reg == 4) // ball
                                    ballPos = curPos;
                                paintedPos[curPos] = (int) reg;
                                Console.SetCursorPosition(0, 0);
                                PrintPanel();
                                Console.WriteLine();
                            }
                        }
                        nOut++;
                        return true;
                        })
                    },
                    { 5, new OpCode(3, /* jit */ delegate() { bool j = Read(Addr(1)) != 0; if (j) pc = Read(Addr(2)); return !j; }) },
                    { 6, new OpCode(3, /* jif */ delegate() { bool j = Read(Addr(1)) == 0; if (j) pc = Read(Addr(2)); return !j; }) },
                    { 7, new OpCode(4, /* lth */ delegate() { Write(Addr(3), Read(Addr(1)) < Read(Addr(2)) ? 1 : 0); return true; }) },
                    { 8, new OpCode(4, /* equ */ delegate() { Write(Addr(3), Read(Addr(1)) == Read(Addr(2)) ? 1 : 0); return true; }) },
                    { 9, new OpCode(2, /* adb */ delegate() { rbase += Read(Addr(1)); return true; }) },
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

            public void Execute()
            {
                instructionsExecuted = 0;
                while (pc >= 0)
                {
                    int opc = (int)mem[pc] % 100;
                    if (opc == 99)
                        return;
                    instructionsExecuted++;
                    if (instructionSet[opc].func())
                        pc += instructionSet[opc].length;
                }
            }

            public void PrintPanel()
            {
                int x0 = paintedPos.Min(a => a.Key.x);
                int x1 = paintedPos.Max(a => a.Key.x);
                int y0 = paintedPos.Min(a => a.Key.y);
                int y1 = paintedPos.Max(a => a.Key.y);
                int w = x1 - x0 + 1;
                int h = y1 - y0 + 1;
                Map map = new Map(w, h, new Position(0, 0), ' ');
                foreach (var kvp in paintedPos)
                {
                    Position p = kvp.Key;
                    char c = ' ';
                    switch (kvp.Value)
                    {
                        case 0: c = ' '; break;
                        case 1: c = '#'; break;
                        case 2: c = '*'; break;
                        case 3: c = '='; break;
                        case 4: c = 'O'; break;
                    }
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

        static void PartA()
        {
            List<long> input = ReadInput();
            IntComputer a = new IntComputer(input, 0);
            a.Execute();
            int res = a.paintedPos.Where(x => x.Value == 2).Count();
            Console.WriteLine("Part A: Result is {0}", res);
        }

        static void PartB()
        {
            List<long> input = ReadInput();
            IntComputer a = new IntComputer(input, 0);
            a.mem[0] = 2;
            a.Execute();
            Console.SetCursorPosition(0, Console.CursorTop + 1);
            Console.WriteLine("Part B: Result is {0}", a.reg);
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2019 - " + typeof(Day13).Namespace + ":");
            PartA();
            PartB();
        }
    }
}
