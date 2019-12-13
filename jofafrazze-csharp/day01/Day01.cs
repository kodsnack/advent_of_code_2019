using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;

namespace day01 
{
    class Day01
    {
        static List<int> ReadInput()
        {
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\..\input.txt");
            StreamReader reader = File.OpenText(path);
            List<int> list = new List<int>();
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                list.Add(int.Parse(line));
            }
            return list;
        }

        static void PartA()
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
        }

        static void PartB()
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
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2019 - " + typeof(Day01).Namespace + ":");
            PartA();
            PartB();
        }
    }
}
