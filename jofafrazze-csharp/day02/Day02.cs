using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;

namespace day02
{
    class Day02
    {
        static List<int> ReadInput()
        {
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\..\input.txt");
            StreamReader reader = File.OpenText(path);
            List<int> list = new List<int>();
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                list.AddRange(line.Split(',').Select(int.Parse).ToList());
            }
            return list;
        }

        static void Add(ref List<int> mem, int pc)
        {
            mem[mem[pc + 3]] = mem[mem[pc + 1]] + mem[mem[pc + 2]];
        }

        static void Mul(ref List<int> mem, int pc)
        {
            mem[mem[pc + 3]] = mem[mem[pc + 1]] * mem[mem[pc + 2]];
        }

        static void RunProgram(ref List<int> mem)
        {
            int pc = 0;
            int opc = 0;
            while (pc >= 0 && pc < mem.Count && opc != 99)
            {
                opc = mem[pc];
                if (opc == 1)
                {
                    Add(ref mem, pc);
                    pc += 4;
                }
                else if (opc == 2)
                {
                    Mul(ref mem, pc);
                    pc += 4;
                }
            }
        }

        static void PartA()
        {
            List<int> input = ReadInput();
            input[1] = 12;
            input[2] = 2;
            RunProgram(ref input);
            Console.WriteLine("Part A: Result is {0}", input[0]);
        }

        static void PartB()
        {
            List<int> input = ReadInput();
            bool done = false;
            int result = -1;
            for (int n = 0; n < 100 && !done; n++)
            {
                for (int v = 0; v < 100 && !done; v++)
                {
                    List<int> mem = new List<int>(input);
                    mem[1] = n;
                    mem[2] = v;
                    RunProgram(ref mem);
                    if (mem[0] == 19690720)
                    {
                        done = true;
                        result = n * 100 + v;
                    }
                }
            }
            Console.WriteLine("Part B: Result is {0}", result);
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2019 - " + typeof(Day02).Namespace + ":");
            PartA();
            PartB();
        }
    }
}
