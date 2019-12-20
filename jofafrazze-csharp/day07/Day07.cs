using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;

using AdventOfCode;

namespace day07
{
    public class Day07
    {
        readonly static string nsname = typeof(Day07).Namespace;

        public class Amplifier
        {
            public List<int> mem;
            public int reg;
            public int phase;
            public int pc;
            public bool firstInput;
            public Amplifier(List<int> mem0, int reg0, int phase0)
            {
                mem = new List<int>(mem0);
                reg = reg0;
                phase = phase0;
                pc = 0;
                firstInput = true;
            }
        }

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

        static bool RunProgram(Amplifier a)
        {
            int opc = 0;
            while (a.pc >= 0 && a.pc < a.mem.Count && opc != 99)
            {
                int v = a.mem[a.pc];
                opc = v % 100;
                bool p1rel = (v / 100) % 10 == 0;
                bool p2rel = (v / 1000) % 10 == 0;
                switch (opc)
                {
                    case 1:
                        Add(ref a.mem, ref a.pc, p1rel, p2rel);
                        break;
                    case 2:
                        Mul(ref a.mem, ref a.pc, p1rel, p2rel);
                        break;
                    case 3:
                        int p = a.firstInput ? a.phase : a.reg;
                        a.firstInput = false;
                        Input(ref a.mem, ref a.pc, p);
                        break;
                    case 4:
                        Output(ref a.mem, ref a.pc, ref a.reg, p1rel);
                        return true;
                    case 5:
                        JumpIfTrue(ref a.mem, ref a.pc, p1rel, p2rel);
                        break;
                    case 6:
                        JumpIfFalse(ref a.mem, ref a.pc, p1rel, p2rel);
                        break;
                    case 7:
                        LessThan(ref a.mem, ref a.pc, p1rel, p2rel);
                        break;
                    case 8:
                        Equals(ref a.mem, ref a.pc, p1rel, p2rel);
                        break;
                }
            }
            return false;
        }

        static bool PartA(Object correctAnswer = null)
        {
            List<int> input = ReadInput();
            List<int> phases = Enumerable.Range(0, 5).ToList();
            var perm = Algorithms.HeapPermutation<int>(phases);
            int maxSignal = int.MinValue;
            foreach (List<int> curPhase in perm)
            {
                int signal = 0;
                for (int i = 0; i < curPhase.Count; i++)
                {
                    Amplifier a = new Amplifier(input, signal, curPhase[i]);
                    RunProgram(a);
                    signal = a.reg;
                }
                if (signal > maxSignal)
                    maxSignal = signal;
            }
            Console.WriteLine("Part A: Result is {0}", maxSignal);
            return correctAnswer == null || maxSignal == (int)correctAnswer;
        }

        static bool PartB(Object correctAnswer = null)
        {
            List<int> input = ReadInput();
            List<int> phases = Enumerable.Range(5, 5).ToList();
            var perm = Algorithms.HeapPermutation<int>(phases);
            int lastOutput = int.MinValue;
            foreach (List<int> curPhase in perm)
            {
                List<Amplifier> amps = new List<Amplifier>();
                for (int i = 0; i < curPhase.Count; i++)
                    amps.Add(new Amplifier(input, 0, curPhase[i]));
                bool keepOn = true;
                while (keepOn)
                {
                    for (int i = 0; (i < curPhase.Count) && keepOn; i++)
                    {
                        keepOn &= RunProgram(amps[i]);
                        amps[(i + 1) % amps.Count].reg = amps[i].reg;
                    }
                    if (keepOn && amps[0].reg > lastOutput)
                        lastOutput = amps[0].reg;
                }
            }
            Console.WriteLine("Part B: Result is {0}", lastOutput);
            return correctAnswer == null || lastOutput == (int)correctAnswer;
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2019 - " + nsname + ":");
            PartA();
            PartB();
        }

        public static bool MainTest()
        {
            int a = 914828;
            int b = 17956613;
            return PartA(a) && PartB(b);
        }
    }
}
