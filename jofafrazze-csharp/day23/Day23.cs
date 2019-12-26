using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace day23
{
    public class Day23
    {
        readonly static string nsname = typeof(Day23).Namespace;

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
            public List<long> input;
            public List<long> output;
            public IntComputer(IntComputer c)
            {
                mem = new Dictionary<long, long>(c.mem);
                reg = c.reg;
                rbase = c.rbase;
                pc = c.pc;
                InitInstructionSet();
                instructionsExecuted = c.instructionsExecuted;
                input = new List<long>(c.input);
                output = new List<long>(c.output);
            }
            public IntComputer(List<long> mem0, long reg0)
            {
                mem = mem0.Select((v, i) => new { v, i }).ToDictionary(x => (long)x.i, x => x.v);
                reg = reg0;
                rbase = 0;
                pc = 0;
                InitInstructionSet();
                instructionsExecuted = 0;
                input = new List<long>();
                output = new List<long>();
            }
            private void InitInstructionSet()
            {
                instructionSet = new Dictionary<int, OpCode>()
                {
                    { 1, new OpCode(4, /* add */ delegate() { Write(Addr(3), Read(Addr(1)) + Read(Addr(2))); return 1; }) },
                    { 2, new OpCode(4, /* mul */ delegate() { Write(Addr(3), Read(Addr(1)) * Read(Addr(2))); return 1; }) },
                    { 3, new OpCode(2, /* inp */ delegate() {
                        if (input.Count > 0)
                        {
                            reg = input.First();
                            input.RemoveAt(0);
                        }
                        else
                            reg = -1;
                        Write(Addr(1), reg);
                        return 2;
                    }) },
                    { 4, new OpCode(2, /* out */ delegate()
                    {
                        reg = Read(Addr(1));
                        output.Add(reg);
                        return 3;
                    }) },
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
                return -1;
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

        static int RunNetworkA(IntComputer computer)
        {
            List<IntComputer> compus = new List<IntComputer>();
            int ans = 0;
            for (int i = 0; i < 50; i++)
            {
                compus.Add(new IntComputer(computer));
                compus.Last().input.Add(i);
            }
            bool done = false;
            while (!done)
            {
                for (int i = 0; i < 50 && !done; i++)
                {
                    IntComputer c = compus[i];
                    if (c.Execute() == 3 && c.output.Count == 3)
                    {
                        int addr = (int)c.output[0];
                        if (addr == 255)
                        {
                            ans = (int)c.output[2];
                            done = true;
                        }
                        else
                        {
                            compus[addr].input.Add(c.output[1]);
                            compus[addr].input.Add(c.output[2]);
                            c.output.Clear();
                        }
                    }
                }
            }
            return ans;
        }

        static int RunNetworkB(IntComputer computer)
        {
            List<IntComputer> compus = new List<IntComputer>();
            List<long> nat = new List<long>();
            int ans = 0;
            for (int i = 0; i < 50; i++)
            {
                compus.Add(new IntComputer(computer));
                compus.Last().input.Add(i);
            }
            bool done = false;
            long lastNatY = 0;
            bool lastNatYSet = false;
            while (!done)
            {
                int sent = 0;
                for (int i = 0; i < 50 && !done; i++)
                {
                    IntComputer c = compus[i];
                    int a = c.Execute();
                    if (a == 3)
                    {
                        sent++;
                        if (c.output.Count == 3)
                        {
                            int addr = (int)c.output[0];
                            if (addr == 255)
                            {
                                nat.Clear();
                                nat.Add(c.output[1]);
                                nat.Add(c.output[2]);
                                c.output.Clear();
                                //Console.WriteLine("NAT received {0}, {1}", nat[0], nat[1]);
                            }
                            else
                            {
                                //Console.WriteLine("C{0} --> C{1}: sent {2}, {3}", i, addr, c.output[1], c.output[2]);
                                compus[addr].input.Add(c.output[1]);
                                compus[addr].input.Add(c.output[2]);
                                c.output.Clear();
                            }
                        }
                    }
                }
                if (nat.Count > 0 && sent == 0)
                {
                    //Console.WriteLine("Nothing sent...");
                    if (compus.Select(w => w.input.Count).Sum() == 0)
                    {
                        if (lastNatYSet && lastNatY == nat[1])
                        {
                            ans = (int)lastNatY;
                            done = true;
                        }
                        compus[0].input.Add(nat[0]);
                        compus[0].input.Add(nat[1]);
                        lastNatY = nat[1];
                        //Console.WriteLine("NAT sent y = {0}", lastNatY);
                        if (!lastNatYSet)
                            lastNatYSet = true;
                        nat.Clear();
                    }
                }
                sent = 0;
            }
            return ans;
        }

        static Object PartA()
        {
            List<long> input = ReadInput();
            IntComputer c0 = new IntComputer(input, 0);
            int a = RunNetworkA(c0);
            Console.WriteLine("Part A: Result is {0}", a);
            return a;
        }

        static Object PartB()
        {
            List<long> input = ReadInput();
            IntComputer c0 = new IntComputer(input, 0);
            int b = RunNetworkB(c0);
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
            int a = 19937;
            int b = 13758;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
