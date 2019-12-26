using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

using Position = AdventOfCode.GenericPosition3D<int>;

namespace day12
{
    public class Day12
    {
        readonly static string nsname = typeof(Day12).Namespace;

        static List<Position> ReadInput()
        {
            string path = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");
            StreamReader reader = File.OpenText(path);
            List<Position> list = new List<Position>();
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                List<string> row = line.Split(',').ToList();
                List<int> n = new List<int>();
                foreach (string s in row)
                {
                    n.Add(int.Parse(s.Split('=').Last().Replace('>', ' ')));
                }
                Position p = new Position(n[0], n[1], n[2]);
                list.Add(p);
            }
            return list;
        }

        static int Adjust(int a, int b)
        {
            return (a < b) ? 1 : (a > b ? -1 : 0);
        }

        static (Position, Position) GetSpeedAdjusts(Position s1, Position s2)
        {
            Position sa = new Position(Adjust(s1.x, s2.x), Adjust(s1.y, s2.y), Adjust(s1.z, s2.z));
            Position sb = new Position(Adjust(s2.x, s1.x), Adjust(s2.y, s1.y), Adjust(s2.z, s1.z));
            return (sa, sb);
        }

        const int bodies = 4;

        static void StepSystem(ref List<Position> pos, ref List<Position> vel)
        {
            List<Position> newVel = new List<Position>(vel);
            for (int a = 0; a < bodies; a++)
            {
                for (int b = a + 1; b < bodies; b++)
                {
                    (Position sa, Position sb) = GetSpeedAdjusts(pos[a], pos[b]);
                    newVel[a] += sa;
                    newVel[b] += sb;
                }
            }
            vel = newVel;
            for (int a = 0; a < bodies; a++)
            {
                pos[a] += vel[a];
            }
        }

        static Object PartA()
        {
            List<Position> input = ReadInput();
            List<Position> pos = new List<Position>(input);
            List<Position> vel = Enumerable.Repeat(new Position(), bodies).ToList();
            for (int i = 0; i < 1000; i++)
            {
                StepSystem(ref pos, ref vel);
            }
            int sum = 0;
            for (int a = 0; a < bodies; a++)
            {
                sum += pos[a].ManhattanDistance() * vel[a].ManhattanDistance();
            }
            Console.WriteLine("Part A: Result is {0}", sum);
            return sum;
        }

        static string GetId(List<Position> pos, List<Position> vel, Position dir)
        {
            string s = "";
            for (int a = 0; a < bodies; a++)
            {
                Position q = pos[a] * dir;
                Position r = vel[a] * dir;
                s += q.x.ToString() + q.y.ToString() + q.z.ToString();
                s += r.x.ToString() + r.y.ToString() + r.z.ToString();
            }
            return s;
        }

        static readonly Position[] units =
        {
            new Position(1, 0, 0),
            new Position(0, 1, 0),
            new Position(0, 0, 1),
        };

        // Createst Common Factor i.e. Createst Common Divisor (GCD)
        public static long GCF(long a, long b)
        {
            while (b != 0)
            {
                long temp = b;
                b = a % b;
                a = temp;
            }
            return a;
        }

        // Least Common Multiple i.e. Lowest Common Denominator (LCD)
        public static long LCM(long a, long b)
        {
            return (a / GCF(a, b)) * b;
        }

        static Object PartB()
        {
            List<Position> input = ReadInput();
            HashSet<string>[] visited = Enumerable.Repeat(new HashSet<string>(), 3).ToArray();
            List<Position> pos = new List<Position>(input);
            List<Position> vel = Enumerable.Repeat(new Position(), bodies).ToList();
            bool done = false;
            int[] cycles = { -1, -1, -1 };
            for (int i = 0; !done; i++)
            {
                StepSystem(ref pos, ref vel);
                done = true;
                for (int a = 0; a < 3; a++)
                {
                    if (cycles[a] < 0)
                    {
                        string id = GetId(pos, vel, units[a]);
                        if (visited[a].Contains(id))
                        {
                            cycles[a] = i;
                            Console.WriteLine("{0}: {1}", a, i);
                        }
                        visited[a].Add(id);
                    }
                    done &= (cycles[a] >= 0);
                }
            }
            long res = LCM(cycles[0], LCM(cycles[1], cycles[2]));
            Console.WriteLine("Part B: Result is {0}", res);
            return res;
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2019 - " + nsname + ":");
            PartA();
            PartB();
        }

        public static bool MainTest()
        {
            int a = 7636;
            long b = 281691380235984;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
