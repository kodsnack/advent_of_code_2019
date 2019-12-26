using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

using AdventOfCode;
using Position = AdventOfCode.GenericPosition2D<int>;

namespace day11
{
    public class Day11
    {
        readonly static string nsname = typeof(Day11).Namespace;

        public class IntComputer
        {
            static readonly Position goUp = new Position(0, -1);
            static readonly Position goRight = new Position(1, 0);
            static readonly Position goDown = new Position(0, 1);
            static readonly Position goLeft = new Position(-1, 0);
            static readonly List<Position> directions = new List<Position>()
            {
                goUp, goRight, goDown, goLeft, 
            };
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
            private Dictionary<int, OpCode> instructionSet;
            public int instructionsExecuted;
            public Position curPos;
            public int dirIndex;
            public Dictionary<Position, int> paintedPos;
            public int nOut;
            public IntComputer(List<long> mem0, long reg0)
            {
                mem = mem0.Select((v, i) => new { v, i }).ToDictionary(x => (long)x.i, x => x.v);
                reg = reg0;
                rbase = 0;
                pc = 0;
                curPos = new Position(0, 0);
                dirIndex = 0;
                paintedPos = new Dictionary<Position, int>();
                nOut = 0;
                instructionSet = new Dictionary<int, OpCode>()
                {
                    { 1, new OpCode(4, /* add */ delegate() { Write(Addr(3), Read(Addr(1)) + Read(Addr(2))); return true; }) },
                    { 2, new OpCode(4, /* mul */ delegate() { Write(Addr(3), Read(Addr(1)) * Read(Addr(2))); return true; }) },
                    { 3, new OpCode(2, /* inp */ delegate() {
                        reg = paintedPos.ContainsKey(curPos) ? paintedPos[curPos] : 0;
                        Write(Addr(1), reg);
                        return true;
                        })
                    },
                    { 4, new OpCode(2, /* out */ delegate() {
                        reg = Read(Addr(1));
                        if (nOut % 2 == 0)
                        {
                            paintedPos[curPos] = (int) reg;
                        }
                        else
                        {
                            int dirAdd = (reg == 0) ? -1 : 1;
                            dirIndex = (dirIndex + dirAdd + 4) % 4;
                            curPos += directions[dirIndex];
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
                return;
            }

            public string PrintPanel()
            {
                int x0 = int.MaxValue;
                int x1 = int.MinValue;
                int y0 = int.MaxValue;
                int y1 = int.MinValue;
                foreach (var kvp in paintedPos)
                {
                    Position p = kvp.Key;
                    if (p.x < x0) x0 = p.x;
                    if (p.x > x1) x1 = p.x;
                    if (p.y < y0) y0 = p.y;
                    if (p.y > y1) y1 = p.y;
                }
                int w = x1 - x0 + 1;
                int h = y1 - y0 + 1;
                Map map = new Map(w, h, new Position(0, 0), ' ');
                foreach (var kvp in paintedPos)
                {
                    Position p = kvp.Key;
                    map.data[p.x - x0, p.y - y0] = (kvp.Value == 0 ? ' ' : '#');
                }
                return map.PrintToString();
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

        static Object PartA()
        {
            List<long> input = ReadInput();
            IntComputer a = new IntComputer(input, 0);
            a.Execute();
            Console.WriteLine("Part A: Result is {0}", a.paintedPos.Count);
            return a.paintedPos.Count;
        }

        static Object PartB()
        {
            List<long> input = ReadInput();
            IntComputer a = new IntComputer(input, 0);
            a.paintedPos[new Position(0, 0)] = 1;
            a.Execute();
            Console.WriteLine("Part B: Result is:");
            string s = a.PrintPanel();
            Console.WriteLine(s);
            return s;
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2019 - " + nsname + ":");
            PartA();
            PartB();
        }

        public static bool MainTest()
        {
            int a = 2293;
            //string b = "AHLCPRAL";
            string b = "  ##  #  # #     ##  ###  ###   ##  #      \r\n"
                     + " #  # #  # #    #  # #  # #  # #  # #      \r\n"
                     + " #  # #### #    #    #  # #  # #  # #      \r\n"
                     + " #### #  # #    #    ###  ###  #### #      \r\n"
                     + " #  # #  # #    #  # #    # #  #  # #      \r\n"
                     + " #  # #  # ####  ##  #    #  # #  # ####   \r\n";
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
