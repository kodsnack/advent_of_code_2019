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

namespace dayXX
{
    class DayXX
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

        static void PartA()
        {
            List<int> input = ReadInput();
            Console.WriteLine("Part A: Result is {0}", 0);
        }

        static void PartB()
        {
            Console.WriteLine("Part B: Result is {0}", 0);
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2019 - " + typeof(DayXX).Namespace + ":");
            PartA();
            PartB();
        }
    }
}
