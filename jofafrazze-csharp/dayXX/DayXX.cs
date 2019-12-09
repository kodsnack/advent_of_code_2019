using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace dayXX
{
    class DayXX
    {
        static int[] ReadInput()
        {
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\..\input.txt");
            StreamReader reader = File.OpenText(path);
            int[] data = { 0, 1, 2 };
            //List<int[]> list = new List<int[]>();
            //string line;
            //while ((line = reader.ReadLine()) != null)
            //{
            //    int[] d = line.Split('x').Select(int.Parse).ToArray();
            //    list.Add(d);
            //}
            return data;
        }

        static void PartA()
        {
            Console.WriteLine("Part A: Result is {0}.", 0);
        }

        static void PartB()
        {
            Console.WriteLine("Part B: Result is {0}.", 0);
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2019 - " + typeof(DayXX).Namespace + ":");
            PartA();
            PartB();
        }
    }
}
