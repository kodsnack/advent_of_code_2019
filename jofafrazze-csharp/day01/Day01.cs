using System;
using System.Collections.Generic;
using System.IO;

namespace day01 
{
    public class Day01
    {
        readonly static string nsname = typeof(Day01).Namespace;

        static List<int> ReadInput()
        {
            string path = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");
            StreamReader reader = File.OpenText(path);
            List<int> list = new List<int>();
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                list.Add(int.Parse(line));
            }
            return list;
        }

        static bool PartA(Object correctAnswer = null)
        {
            List<int> input = ReadInput();
            int i = 0;
            int sum = 0;
            while (i >= 0 && i < input.Count)
            {
                int add = input[i];
                sum += (add / 3) - 2;
                i++;
            }
            Console.WriteLine("Part A: Result is {0}", sum);
            return correctAnswer == null || sum == (int)correctAnswer;
        }

        static bool PartB(Object correctAnswer = null)
        {
            List<int> input = ReadInput();
            int i = 0;
            int sum = 0;
            while (i >= 0 && i < input.Count)
            {
                int w = input[i];
                do
                {
                    w = (w / 3) - 2;
                    sum += Math.Max(0, w);
                }
                while (w > 0);
                i++;
            }
            Console.WriteLine("Part B: Result is {0}", sum);
            return correctAnswer == null || sum == (int)correctAnswer;
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2019 - " + nsname + ":");
            PartA();
            PartB();
        }

        public static bool MainTest()
        {
            int a = 3384232;
            int b = 5073456;
            return PartA(a) && PartB(b);
        }
    }
}
