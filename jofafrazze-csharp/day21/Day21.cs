using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

//using AdventOfCode;
//using Position = AdventOfCode.GenericPosition2D<int>;

namespace day21
{
    public class Day21
    {
        readonly static string nsname = typeof(Day21).Namespace;

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

        static bool PartA(Object correctAnswer = null)
        {
            List<int> input = ReadInput();
            int ans = 0;
            Console.WriteLine("Part A: Result is {0}", ans);
            return correctAnswer == null || ans == (int)correctAnswer;
        }

        static bool PartB(Object correctAnswer = null)
        {
            int ans = 0;
            Console.WriteLine("Part B: Result is {0}", ans);
            return correctAnswer == null || ans == (int)correctAnswer;
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
