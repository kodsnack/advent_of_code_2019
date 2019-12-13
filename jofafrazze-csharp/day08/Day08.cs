using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;

using AdventOfCode;
using Position = AdventOfCode.GenericPosition2D<int>;

namespace day08
{
    class Day08
    {
        static List<int> ReadInput()
        {
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\..\input.txt");
            StreamReader reader = File.OpenText(path);
            string line = reader.ReadLine();
            return line.ToCharArray().Select(i => int.Parse(i.ToString())).ToList();
        }

        static readonly int w = 25;
        static readonly int h = 6;
        static readonly int size = w * h;

        static void PartA()
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
        }

        static readonly Dictionary<int, char> dict = new Dictionary<int, char>()
        {
            { 0, ' ' },
            { 1, '*' },
            { 2, 't' },
        };

        static void PartB()
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
            map.Print();
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2019 - " + typeof(Day08).Namespace + ":");
            PartA();
            PartB();
        }
    }
}
