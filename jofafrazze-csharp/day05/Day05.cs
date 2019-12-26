using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace day05
{
    public class Day05
    {
        readonly static string nsname = typeof(Day05).Namespace;

        static List<int> ReadInput()
        {
            string path = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");
            StreamReader reader = File.OpenText(path);
            List<int> list = new List<int>();
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                list.AddRange(line.Split(',').Select(int.Parse).ToList());
            }
            return list;
        }

        static void Add(ref List<int> mem, ref int pc, bool p1rel, bool p2rel)
        {
            int a = p1rel ? mem[mem[pc + 1]] : mem[pc + 1];
            int b = p2rel ? mem[mem[pc + 2]] : mem[pc + 2];
            mem[mem[pc + 3]] = a + b;
            pc += 4;
        }

        static void Mul(ref List<int> mem, ref int pc, bool p1rel, bool p2rel)
        {
            int a = p1rel ? mem[mem[pc + 1]] : mem[pc + 1];
            int b = p2rel ? mem[mem[pc + 2]] : mem[pc + 2];
            mem[mem[pc + 3]] = a * b;
            pc += 4;
        }

        static void Input(ref List<int> mem, ref int pc, int reg)
        {
            mem[mem[pc + 1]] = reg;
            pc += 2;
        }

        static void Output(ref List<int> mem, ref int pc, ref int reg, bool p1rel)
        {
            reg = p1rel ? mem[mem[pc + 1]] : mem[pc + 1];
            pc += 2;
        }

        static void JumpIfTrue(ref List<int> mem, ref int pc, bool p1rel, bool p2rel)
        {
            int a = p1rel ? mem[mem[pc + 1]] : mem[pc + 1];
            int b = p2rel ? mem[mem[pc + 2]] : mem[pc + 2];
            if (a != 0)
                pc = b;
            else
                pc += 3;
        }

        static void JumpIfFalse(ref List<int> mem, ref int pc, bool p1rel, bool p2rel)
        {
            int a = p1rel ? mem[mem[pc + 1]] : mem[pc + 1];
            int b = p2rel ? mem[mem[pc + 2]] : mem[pc + 2];
            if (a == 0)
                pc = b;
            else
                pc += 3;
        }

        static void LessThan(ref List<int> mem, ref int pc, bool p1rel, bool p2rel)
        {
            int a = p1rel ? mem[mem[pc + 1]] : mem[pc + 1];
            int b = p2rel ? mem[mem[pc + 2]] : mem[pc + 2];
            int v = (a < b) ? 1 : 0;
            mem[mem[pc + 3]] = v;
            pc += 4;
        }

        static void Equals(ref List<int> mem, ref int pc, bool p1rel, bool p2rel)
        {
            int a = p1rel ? mem[mem[pc + 1]] : mem[pc + 1];
            int b = p2rel ? mem[mem[pc + 2]] : mem[pc + 2];
            int v = (a == b) ? 1 : 0;
            mem[mem[pc + 3]] = v;
            pc += 4;
        }

        static void RunProgram(ref List<int> mem, ref int reg)
        {
            int pc = 0;
            int opc = 0;
            while (pc >= 0 && pc < mem.Count && opc != 99)
            {
                int v = mem[pc];
                opc = v % 100;
                bool p1rel = (v / 100) % 10 == 0;
                bool p2rel = (v / 1000) % 10 == 0;
                switch (opc)
                {
                    case 1:
                        Add(ref mem, ref pc, p1rel, p2rel);
                        break;
                    case 2:
                        Mul(ref mem, ref pc, p1rel, p2rel);
                        break;
                    case 3:
                        Input(ref mem, ref pc, reg);
                        break;
                    case 4:
                        Output(ref mem, ref pc, ref reg, p1rel);
                        break;
                    case 5:
                        JumpIfTrue(ref mem, ref pc, p1rel, p2rel);
                        break;
                    case 6:
                        JumpIfFalse(ref mem, ref pc, p1rel, p2rel);
                        break;
                    case 7:
                        LessThan(ref mem, ref pc, p1rel, p2rel);
                        break;
                    case 8:
                        Equals(ref mem, ref pc, p1rel, p2rel);
                        break;
                }
            }
        }

        static Object PartA()
        {
            List<int> input = ReadInput();
            int reg = 1;
            RunProgram(ref input, ref reg);
            Console.WriteLine("Part A: Result is {0}", reg);
            return reg;
        }

        static Object PartB()
        {
            List<int> input = ReadInput();
            int reg = 5;
            RunProgram(ref input, ref reg);
            Console.WriteLine("Part B: Result is {0}", reg);
            return reg;
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2019 - " + nsname + ":");
            PartA();
            PartB();
        }

        public static bool MainTest()
        {
            int a = 7692125;
            int b = 14340395;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
