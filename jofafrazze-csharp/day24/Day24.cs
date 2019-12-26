using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

using AdventOfCode;
using Position = AdventOfCode.GenericPosition3D<int>;

namespace day24
{
    public class Day24
    {
        readonly static string nsname = typeof(Day24).Namespace;

        static readonly Position goUp = new Position(0, -1, 0);
        static readonly Position goRight = new Position(1, 0, 0);
        static readonly Position goDown = new Position(0, 1, 0);
        static readonly Position goLeft = new Position(-1, 0, 0);
        static readonly List<Position> neighbours = new List<Position>()
        {
            goUp,
            goLeft, goRight,
            goDown,
        };

        public class OurMap
        {
            public Dictionary<Position, char> mapPos;
            public OurMap()
            {
                mapPos = new Dictionary<Position, char>();
            }
            public OurMap(OurMap m)
            {
                mapPos = new Dictionary<Position, char>(m.mapPos);
            }

            public void PrintMap()
            {
                if (mapPos.Count == 0)
                    return;
                int x0 = mapPos.Min(a => a.Key.x);
                int x1 = mapPos.Max(a => a.Key.x);
                int y0 = mapPos.Min(a => a.Key.y);
                int y1 = mapPos.Max(a => a.Key.y);
                int w = x1 - x0 + 1;
                int h = y1 - y0 + 1;
                Map map = new Map(w, h, new AdventOfCode.GenericPosition2D<int>(0, 0), '.');
                int z0 = mapPos.Min(a => a.Key.z);
                int z1 = mapPos.Max(a => a.Key.z);
                for (int z = z0; z <= z1; z++)
                {
                    Console.WriteLine("Depth {0}", z);
                    foreach (var kvp in mapPos.Where(a => a.Key.z == z))
                    {
                        Position p = kvp.Key;
                        map.data[p.x - x0, p.y - y0] = kvp.Value;
                    }
                    map.Print();
                    Console.WriteLine();
                }
            }
        }

        static List<string> ReadInput()
        {
            string path = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");
            StreamReader reader = File.OpenText(path);
            List<string> list = new List<string>();
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                list.Add(line);
            }
            return list;
        }

        static OurMap BuildMap(List<string> list)
        {
            int w = list[0].Length;
            int h = list.Count;
            OurMap m = new OurMap();
            for (int y = 0; y < h; y++)
            {
                for (int x = 0; x < w; x++)
                {
                    char c = list[y][x];
                    m.mapPos[new Position(x, y, 0)] = c;
                }
            }
            return m;
        }

        static int CalculateNeighbours2D(OurMap m, Position p)
        {
            int n = 0;
            foreach (Position d in neighbours)
            {
                Position q = p + d;
                if (m.mapPos.ContainsKey(q) && m.mapPos[q] == '#')
                    n++;
            }
            return n;
        }

        static int IterateMap2D(ref OurMap m)
        {
            int id = 0;
            OurMap next = new OurMap(m);
            for (int y = 4; y >= 0; y--)
            {
                for (int x = 4; x >= 0; x--)
                {
                    id <<= 1;
                    Position p = new Position(x, y, 0);
                    int n = CalculateNeighbours2D(m, p);
                    if (n != 1 && m.mapPos[p] == '#')
                        next.mapPos[p] = '.';
                    if ((n == 1 || n == 2) && m.mapPos[p] == '.')
                        next.mapPos[p] = '#';
                    if (next.mapPos[p] == '#')
                        id |= 1;
                }
            }
            m = next;
            return id;
        }

        static int CalculateNeighbours3D(OurMap m, Position p)
        {
            List<Position> n = new List<Position>();

            // left neighbour(s)
            if (p.x == 0)
                n.Add(new Position(1, 2, p.z - 1));
            else if (p.x == 3 && p.y == 2)
            {
                for (int i = 0; i < 5; i++)
                    n.Add(new Position(4, i, p.z + 1));
            }
            else
                n.Add(p + goLeft);

            // right neighbour(s)
            if (p.x == 4)
                n.Add(new Position(3, 2, p.z - 1));
            else if (p.x == 1 && p.y == 2)
            {
                for (int i = 0; i < 5; i++)
                    n.Add(new Position(0, i, p.z + 1));
            }
            else
                n.Add(p + goRight);

            // up neighbour(s)
            if (p.y == 0)
                n.Add(new Position(2, 1, p.z - 1));
            else if (p.y == 3 && p.x == 2)
            {
                for (int i = 0; i < 5; i++)
                    n.Add(new Position(i, 4, p.z + 1));
            }
            else
                n.Add(p + goUp);

            // down neighbour(s)
            if (p.y == 4)
                n.Add(new Position(2, 3, p.z - 1));
            else if (p.y == 1 && p.x == 2)
            {
                for (int i = 0; i < 5; i++)
                    n.Add(new Position(i, 0, p.z + 1));
            }
            else
                n.Add(p + goDown);

            return n.Where(w => m.mapPos.ContainsKey(w) && m.mapPos[w] == '#').Count();
        }

        static void IterateMap3D(ref OurMap m)
        {
            OurMap next = new OurMap(m);
            int z0 = m.mapPos.Min(w => w.Key.z) - 1;
            int z1 = m.mapPos.Max(w => w.Key.z) + 1;
            for (int z = z0; z <= z1; z++)
            {
                for (int y = 0; y < 5; y++)
                {
                    for (int x = 0; x < 5; x++)
                    {
                        Position p = new Position(x, y, z);
                        int n = CalculateNeighbours3D(m, p);
                        if (n != 1 && m.mapPos.ContainsKey(p) && m.mapPos[p] == '#')
                            next.mapPos[p] = '.';
                        if ((n == 1 || n == 2) && (!m.mapPos.ContainsKey(p) || m.mapPos[p] == '.'))
                            next.mapPos[p] = '#';
                    }
                }
            }
            m = next;
        }

        static Object PartA()
        {
            List<string> input = ReadInput();
            OurMap m = BuildMap(input);
            HashSet<int> visited = new HashSet<int>();
            int rating = -1;
            while (!visited.Contains(rating))
            {
                //Console.SetCursorPosition(0, 1);
                //m.PrintMap();
                visited.Add(rating);
                rating = IterateMap2D(ref m);
            }
            Console.WriteLine("Part A: Result is {0}", rating);
            return rating;
        }

        static Object PartB()
        {
            List<string> input = ReadInput();
            OurMap m = BuildMap(input);
            //m.PrintMap();
            for (int i = 0; i < 200; i++)
            {
                IterateMap3D(ref m);
            }
            //m.PrintMap();
            int ans = 0;
            foreach (var kvp in m.mapPos)
            {
                if (kvp.Value == '#' && !(kvp.Key.x == 2 && kvp.Key.y == 2))
                    ans++;
            }
            Console.WriteLine("Part B: Result is {0}", ans);
            return ans;
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2019 - " + nsname + ":");
            PartA();
            PartB();
        }

        public static bool MainTest()
        {
            int a = 17863741;
            int b = 2029;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
