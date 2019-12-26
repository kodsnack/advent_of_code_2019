using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace day21
{
    public class Day21
    {
        readonly static string nsname = typeof(Day21).Namespace;

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

        // (!A + !B + !C) * D
        static readonly string springScriptA =
            "NOT A T\n" +
            "OR T J\n" +
            "NOT B T\n" +
            "OR T J\n" +
            "NOT C T\n" +
            "OR T J\n" +
            "AND D J\n" +
            "WALK\n";

        // (!A + !B + !C) * D * (H + (E * I) + (E * F))
        // ...rewritten as...
        // !(A * B * C) * D * (E + H) * (F + H + I)
        static readonly string springScriptB =
            "NOT A J\n" +
            "NOT J J\n" +
            "AND B J\n" +
            "AND C J\n" +
            "NOT J J\n" +
            "AND D J\n" +
            "OR E T\n" +
            "OR H T\n" +
            "AND T J\n" +
            "NOT F T\n" +
            "NOT T T\n" +
            "OR H T\n" +
            "OR I T\n" +
            "AND T J\n" +
            "RUN\n";

        static long RunDroid(IntComputer ic, string s)
        {
            int i = 0;
            int ret = 0;
            long lastOut = 0;
            do
            {
                lastOut = ic.reg;
                ic.reg = s[Math.Min(i, s.Length - 1)];
                ret = ic.Execute();
                if (ret == 2)
                {
                    i++;
                }
                else if (ret == 3)
                {
                    if (ic.reg < 255)
                        Console.Write((char)ic.reg);
                }
            }
            while (ret > 0);
            return lastOut;
        }

        static Object PartA()
        {
            List<long> input = ReadInput();
            IntComputer c0 = new IntComputer(input, 0);
            long a = RunDroid(c0, springScriptA);
            Console.WriteLine("Part A: Result is {0}", a);
            return a;
        }

        static Object PartB()
        {
            List<long> input = ReadInput();
            IntComputer c0 = new IntComputer(input, 0);
            long b = RunDroid(c0, springScriptB);
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
            long a = 19358870;
            long b = 1143356492;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
