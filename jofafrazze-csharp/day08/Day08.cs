using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;

using AdventOfCode;
using Position = AdventOfCode.GenericPosition2D<int>;

namespace day08
{
    public class Day08
    {
        readonly static string nsname = typeof(Day08).Namespace;

        static List<int> ReadInput()
        {
            string path = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");
            StreamReader reader = File.OpenText(path);
            string line = reader.ReadLine();
            return line.ToCharArray().Select(i => int.Parse(i.ToString())).ToList();
        }

        static readonly int w = 25;
        static readonly int h = 6;
        static readonly int size = w * h;

        static bool PartA(Object correctAnswer = null)
        {
            List<int> input = ReadInput();
            int nZeros = int.MaxValue;
            int resOffs = -1;
            for (int offs = 0; offs < input.Count - 1; offs += size)
            {
                List<int> a = input.GetRange(offs, size);
                int n = a.Where(i => i == 0).Count();
                if (n < nZeros)
                {
                    nZeros = n;
                    resOffs = offs;
                }
            }
            List<int> l = input.GetRange(resOffs, size);
            int sum = l.Where(i => i == 1).Count() * l.Where(i => i == 2).Count();
            Console.WriteLine("Part A: Result is {0}", sum);
            return correctAnswer == null || sum == (int)correctAnswer;
        }

        static readonly Dictionary<int, char> dict = new Dictionary<int, char>()
        {
            { 0, ' ' },
            { 1, '*' },
            { 2, 't' },
        };

        static bool PartB(Object correctAnswer = null)
        {
            List<int> input = ReadInput();
            Map map = new Map(w, h, new Position(0, 0), dict[2]);
            for (int offs = 0; offs < input.Count - 1; offs += size)
            {
                List<int> a = input.GetRange(offs, size);
                for (int y = 0; y < h; y++)
                {
                    for (int x = 0; x < w; x++)
                    {
                        if (map.data[x, y] == dict[2])
                            map.data[x, y] = dict[a[y * w + x]];
                    }
                }
                //map.Print();
                //Console.WriteLine();
            }
            Console.WriteLine("Part B: Result is:");
            string s = map.PrintToString();
            Console.WriteLine(s);
            return correctAnswer == null || s == (string)correctAnswer;
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2019 - " + nsname + ":");
            PartA();
            PartB();
        }

        public static bool MainTest()
        {
            int a = 1965;
            //string b = "GZKJY";
            string b = " **  **** *  *   ** *   *\r\n"
                     + "*  *    * * *     * *   *\r\n"
                     + "*      *  **      *  * * \r\n"
                     + "* **  *   * *     *   *  \r\n"
                     + "*  * *    * *  *  *   *  \r\n"
                     + " *** **** *  *  **    *  \r\n";
            return PartA(a) && PartB(b);
        }
    }
}
