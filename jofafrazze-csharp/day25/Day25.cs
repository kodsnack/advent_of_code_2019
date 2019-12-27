using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

using AdventOfCode;
using Position = AdventOfCode.GenericPosition2D<int>;

namespace day25
{
    public class Day25
    {
        readonly static string nsname = typeof(Day25).Namespace;

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

        static List<string> ReadInitialDirections()
        {
            string path = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\initial_directions.txt");
            StreamReader reader = File.OpenText(path);
            List<string> list = new List<string>();
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                list.Add(line);
            }
            return list;
        }

        static void LogInput(string s, bool log)
        {
            if (!log)
                return;
            string path = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\directions.txt");
            using (StreamWriter writer = new StreamWriter(new FileStream(path, FileMode.Append)))
            {
                if (s.Length > 1)
                    writer.Write(s);
            }
        }

        static void LogAll(string s, bool log)
        {
            if (!log)
                return;
            string path = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\full_log.txt");
            using (StreamWriter writer = new StreamWriter(new FileStream(path, FileMode.Append)))
            {
                writer.Write(s);
            }
        }

        static int SolveTextAdventure(IntComputer ic, List<string> directions, bool log)
        {
            string cmd = "Command?";
            string input = "";
            string output = "";
            string lastOutput = "";
            int ret = 0;
            int i = 0;
            int row = 0;
            LogAll("\n============================= NEW SESSION =============================\n\n", log);
            do
            {
                if (i < input.Length)
                    ic.reg = input[i];
                ret = ic.Execute();
                if (ret == 2)
                {
                    i++;
                }
                else if (ret == 3)
                {
                    char c = (char)ic.reg;
                    if (c == '\n')
                    {
                        Console.WriteLine(output);
                        LogAll(output + '\n', log);
                        if (output == cmd)
                        {
                            if (row < directions.Count)
                            {
                                input = directions[row] + '\n';
                                Console.Write(input);
                                row++;
                            }
                            else
                                input = Console.ReadLine() + '\n';
                            LogInput(input, log);
                            LogAll(input, log);
                            i = 0;
                        }
                        if (output.Length > 0)
                            lastOutput = output;
                        output = "";
                    }
                    else
                    {
                        output += c;
                    }
                }
            }
            while (ret > 0);
            int res = -1;
            foreach (string s in lastOutput.Split(' '))
                if (int.TryParse(s, out res))
                    break;
            return res;
        }

        static readonly List<string> inventories = new List<string>() {
            "weather machine",
            "bowl of rice",
            "polygon",
            "hypercube",
            "dark matter",
            "candy cane",
            "manifold",
            "dehydrated water"
        };

        static readonly List<string> apparentlyCorrectInventories = new List<string>() {
            "bowl of rice",
            "dark matter",
            "candy cane",
            "dehydrated water"
        };

        static List<string> TryInventories(List<List<string>> takeCmdsList)
        {
            List<string> list = new List<string>();
            List<string> dropCmds = inventories.Select(w => "drop " + w).ToList();
            foreach (List<string> takeCmds in takeCmdsList)
            {
                list.AddRange(dropCmds);
                list.AddRange(takeCmds);
                list.Add("inv");
                list.Add("south");
            }
            return list;
        }

        static Object PartA()
        {
            List<long> input = ReadInput();
            IntComputer c0 = new IntComputer(input, 0);
            List<string> directions = ReadInitialDirections();
            //List<string> takeCmds = inventories.Select(w => "take " + w).ToList();
            //var combos = AdventOfCode.Algorithms.GetCombinations(takeCmds);
            List<string> takeCmds = apparentlyCorrectInventories.Select(w => "take " + w).ToList();
            List<List<string>> combos = new List<List<string>>() { takeCmds };
            directions.AddRange(TryInventories(combos));
            int ans = SolveTextAdventure(c0, directions, false);
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2019 - " + nsname + ":");
            PartA();
        }

        public static bool MainTest()
        {
            int a = 10504192;
            return PartA().Equals(a);
        }
    }
}
