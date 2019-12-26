using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

using Position = AdventOfCode.GenericPosition2D<int>;

namespace day03
{
    public class Day03
    {
        readonly static string nsname = typeof(Day03).Namespace;

        static (List<string>, List<string>) ReadInput()
        {
            string path = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");
            StreamReader reader = File.OpenText(path);
            List<string> list1 = reader.ReadLine().Split(',').ToList();
            List<string> list2 = reader.ReadLine().Split(',').ToList();
            return (list1, list2);
        }

        static readonly Dictionary<char, Position> directions = new Dictionary<char, Position>()
        {
            { 'U', new Position(0, -1) },
            { 'R', new Position(1, 0) },
            { 'D', new Position(0, 1) },
            { 'L', new Position(-1, 0) },
        };

        static Object PartA()
        {
            (List<string> path1, List<string> path2) = ReadInput();
            HashSet<Position> p1visited = new HashSet<Position>();
            Position center = new Position(0, 0);
            Position pos = center;
            foreach(string d in path1)
            {
                int l = int.Parse(d.Substring(1));
                Position dir = directions[d[0]];
                for (int i = 0; i < l; i++)
                {
                    pos += dir;
                    p1visited.Add(pos);
                }
            }
            pos = center;
            int minDist = int.MaxValue;
            foreach (string d in path2)
            {
                int l = int.Parse(d.Substring(1));
                Position dir = directions[d[0]];
                for (int i = 0; i < l; i++)
                {
                    pos += dir;
                    if (p1visited.Contains(pos))
                    {
                        int dist = pos.ManhattanDistance(center);
                        if (dist < minDist)
                            minDist = dist;
                    }
                }
            }
            Console.WriteLine("Part A: Result is {0}", minDist);
            return minDist;
        }

        static Object PartB()
        {
            (List<string> path1, List<string> path2) = ReadInput();
            Dictionary<Position, int> p1steps = new Dictionary<Position, int>();
            Position center = new Position(0, 0);
            Position pos = center;
            int steps = 0;
            foreach (string d in path1)
            {
                int l = int.Parse(d.Substring(1));
                Position dir = directions[d[0]];
                for (int i = 0; i < l; i++)
                {
                    pos += dir;
                    steps++;
                    if (!p1steps.ContainsKey(pos))
                        p1steps[pos] = steps;
                }
            }
            pos = center;
            int minSum = int.MaxValue;
            steps = 0;
            foreach (string d in path2)
            {
                int l = int.Parse(d.Substring(1));
                Position dir = directions[d[0]];
                for (int i = 0; i < l; i++)
                {
                    pos += dir;
                    steps++;
                    if (p1steps.ContainsKey(pos))
                    {
                        int sum = p1steps[pos] + steps;
                        if (sum < minSum)
                            minSum= sum;
                    }
                }
            }
            Console.WriteLine("Part B: Result is {0}", minSum);
            return minSum;
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2019 - " + nsname + ":");
            PartA();
            PartB();
        }

        public static bool MainTest()
        {
            int a = 399;
            int b = 15678;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
