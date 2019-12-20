using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;

using AdventOfCode;
using Position = AdventOfCode.GenericPosition2D<int>;

namespace day18
{
    public class Day18
    {
        readonly static string nsname = typeof(Day18).Namespace;

        static readonly Position goUp = new Position(0, -1);
        static readonly Position goRight = new Position(1, 0);
        static readonly Position goDown = new Position(0, 1);
        static readonly Position goLeft = new Position(-1, 0);
        static readonly List<Position> directions = new List<Position>()
        {
            goUp, goRight, goDown, goLeft,
        };

        public class MapSearchInfo
        {
            public Position[] positions;
            public SortedSet<char> found;
            public int steps;
            public MapSearchInfo(Position[] p, SortedSet<char> f, int s)
            {
                positions = new Position[p.Length];
                p.CopyTo(positions, 0);
                found = new SortedSet<char>(f);
                steps = s;
            }
            public MapSearchInfo(MapSearchInfo m)
            {
                positions = new Position[m.positions.Length];
                m.positions.CopyTo(positions, 0);
                found = new SortedSet<char>(m.found);
                steps = m.steps;
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

        static Map BuildMap(List<string> list)
        {
            int w = list[0].Length;
            int h = list.Count;
            Map m = new Map(w, h, new Position(0, 0));
            for (int y = 0; y < h; y++)
                for (int x = 0; x < w; x++)
                    m.data[x, y] = list[y][x];
            return m;
        }
        
        static Dictionary<char, (int, int)> FindDirectlyReachableKeyDistances(Map m, MapSearchInfo msi)
        {
            List<Position> toVisit = new List<Position>(msi.positions);
            Dictionary<Position, (int steps, int idx)> steps = new Dictionary<Position, (int, int)>();
            for (int i = 0; i < msi.positions.Length; i++)
                steps[msi.positions[i]] = (0, i);
            List<Position> keyPos = new List<Position>();
            while (toVisit.Count > 0)
            {
                List<Position> toVisitNext = new List<Position>();
                foreach (Position p in toVisit)
                {
                    foreach (Position d in directions)
                    {
                        Position nextPos = p + d;
                        char c = m[nextPos];
                        if (c != '#')
                        {
                            if (c == '.' || c == '@' || msi.found.Contains(c) || char.IsLower(c))
                            {
                                (int steps, int idx) s = (steps[p].steps + 1, steps[p].idx);
                                if (!steps.ContainsKey(nextPos) || s.steps < steps[nextPos].steps)
                                {
                                    steps[nextPos] = s;
                                    if (c == '.' || c == '@' || msi.found.Contains(c))
                                        toVisitNext.Add(nextPos);
                                    else
                                        keyPos.Add(nextPos);
                                }
                            }
                        }
                    }
                }
                toVisit = toVisitNext;
            }
            Dictionary<char, (int, int)> foundChars = new Dictionary<char, (int, int)>();
            foreach (Position p in keyPos)
                foundChars[m[p]] = steps[p];
            return foundChars;
        }

        static bool FindInMap(Map m, char c, ref Position p)
        {
            for (int y = 0; y < m.height; y++)
            {
                for (int x = 0; x < m.width; x++)
                {
                    if (m.data[x, y] == c)
                    {
                        p = new Position(x, y);
                        return true;
                    }
                }
            }
            return false;
        }

        static void PrintInfoSteps(Dictionary<string, MapSearchInfo> infoSteps)
        {
            //Console.WriteLine("Found {0} different paths:", infoSteps.Count());
            //foreach (var v in infoSteps)
            //{
            //    Console.WriteLine("[{0}, {1}]: {2} in {3} steps.", 
            //        v.Value.positions[0].x, v.Value.positions[0].y,
            //        v.Key, v.Value.steps);
            //}
            //Console.WriteLine();
            Console.Write(".");
        }

        static string CreateStr(MapSearchInfo msi)
        {
            string ps = "";
            foreach (Position p in msi.positions)
                ps += p.x.ToString() + ',' + p.y.ToString() + ',';
            return string.Join("", msi.found) + ps;
        }

        static int CollectAllKeys(Map m, List<Position> positions)
        {
            Dictionary<string, MapSearchInfo> infoSteps = new Dictionary<string, MapSearchInfo>();
            MapSearchInfo msi0 = new MapSearchInfo(positions.ToArray(), new SortedSet<char>(), 0);
            infoSteps[CreateStr(msi0)] = msi0;
            int minSteps = int.MaxValue;
            while (infoSteps.Count > 0)
            {
                PrintInfoSteps(infoSteps);
                Dictionary<string, MapSearchInfo> nextInfoSteps = new Dictionary<string, MapSearchInfo>();
                foreach (var v in infoSteps)
                {
                    Dictionary<char, (int steps, int idx)> keyDists = FindDirectlyReachableKeyDistances(m, v.Value);
                    if (keyDists.Count == 0)
                    {
                        if (v.Value.steps < minSteps)
                            minSteps = v.Value.steps;
                    }
                    foreach (var kvp in keyDists)
                    {
                        MapSearchInfo nextMsi = new MapSearchInfo(v.Value);
                        nextMsi.steps += kvp.Value.steps;
                        FindInMap(m, kvp.Key, ref nextMsi.positions[kvp.Value.idx]);
                        nextMsi.found.Add(kvp.Key);
                        Position p = new Position();
                        char c2 = char.ToUpper(kvp.Key);
                        if (FindInMap(m, c2, ref p))
                            nextMsi.found.Add(c2);
                        string sid = CreateStr(nextMsi);
                        if (!nextInfoSteps.ContainsKey(sid) || nextInfoSteps[sid].steps > nextMsi.steps)
                            nextInfoSteps[sid] = nextMsi;
                    }
                }
                infoSteps = nextInfoSteps;
            }
            Console.WriteLine();
            return minSteps;
        }

        static bool PartA(Object correctAnswer = null)
        {
            List<string> input = ReadInput();
            Map m = BuildMap(input);
            m.Print();
            Position p = new Position();
            FindInMap(m, '@', ref p);
            int a = CollectAllKeys(m, new List<Position>() { p });
            Console.WriteLine("Part A: Result is {0}", a);
            return correctAnswer == null || a == (int)correctAnswer;
        }

        static bool PartB(Object correctAnswer = null)
        {
            List<string> input = ReadInput();
            Map m = BuildMap(input);
            Position p = new Position();
            FindInMap(m, '@', ref p);
            if (m.data.Cast<char>().Count(a => a == '@') == 1)
            {
                p = new Position(p.x - 1, p.y - 1);
                m.data[p.x + 0, p.y + 0] = '@';
                m.data[p.x + 1, p.y + 0] = '#';
                m.data[p.x + 2, p.y + 0] = '@';
                m.data[p.x + 0, p.y + 1] = '#';
                m.data[p.x + 1, p.y + 1] = '#';
                m.data[p.x + 2, p.y + 1] = '#';
                m.data[p.x + 0, p.y + 2] = '@';
                m.data[p.x + 1, p.y + 2] = '#';
                m.data[p.x + 2, p.y + 2] = '@';
            }
            //m.Print();
            FindInMap(m, '@', ref p);
            List<Position> posList = new List<Position>() { p };
            posList.Add(new Position(p.x + 2, p.y + 0));
            posList.Add(new Position(p.x + 0, p.y + 2));
            posList.Add(new Position(p.x + 2, p.y + 2));
            int b = CollectAllKeys(m, posList);
            Console.WriteLine("Part B: Result is {0}", b);
            return correctAnswer == null || b == (int)correctAnswer;
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2019 - " + nsname + ":");
            PartA();
            PartB();
        }

        public static bool MainTest()
        {
            int a = 4192;
            int b = 1790;
            return PartA(a) && PartB(b);
        }
    }
}
