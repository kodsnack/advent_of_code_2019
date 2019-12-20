using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;

namespace day09
{
    public class Day09
    {
        readonly static string nsname = typeof(Day09).Namespace;

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
            private Dictionary<int, OpCode> instructionSet;
            public int instructionsExecuted;
            public IntComputer(List<long> mem0, long reg0)
            {
                mem = mem0.Select((v, i) => new { v, i }).ToDictionary(x => (long)x.i, x => x.v);
                reg = reg0;
                rbase = 0;
                pc = 0;
                instructionSet = new Dictionary<int, OpCode>()
                {
                    { 1, new OpCode(4, /* add */ delegate() { Write(Addr(3), Read(Addr(1)) + Read(Addr(2))); return true; }) },
                    { 2, new OpCode(4, /* mul */ delegate() { Write(Addr(3), Read(Addr(1)) * Read(Addr(2))); return true; }) },
                    { 3, new OpCode(2, /* inp */ delegate() { Write(Addr(1), reg); return true; }) },
                    { 4, new OpCode(2, /* out */ delegate() { reg = Read(Addr(1)); Console.Write("{0},", reg); return true; }) },
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

        static bool PartA(Object correctAnswer = null)
        {
            List<long> input = ReadInput();
            IntComputer a = new IntComputer(input, 1);
            a.Execute();
            Console.WriteLine();
            Console.WriteLine("Part A: Result is {0}", a.reg);
            return correctAnswer == null || a.reg == (long)correctAnswer;
        }

        static bool PartB(Object correctAnswer = null)
        {
            List<long> input = ReadInput();
            IntComputer a = new IntComputer(input, 2);
            a.Execute();
            Console.WriteLine();
            Console.WriteLine("Part B: Result is {0}", a.reg);
            return correctAnswer == null || a.reg == (long)correctAnswer;
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2019 - " + nsname + ":");
            PartA();
            PartB();
        }

        public static bool MainTest()
        {
            long a = 3280416268;
            long b = 80210;
            return PartA(a) && PartB(b);
        }
    }
}
