using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;

using AdventOfCode;
using Position = AdventOfCode.GenericPosition2D<int>;

namespace day20
{
    class Day20
    {
        static readonly Position goUp = new Position(0, -1);
        static readonly Position goRight = new Position(1, 0);
        static readonly Position goDown = new Position(0, 1);
        static readonly Position goLeft = new Position(-1, 0);
        static readonly List<Position> directions = new List<Position>()
        {
            goUp, goRight, goDown, goLeft,
        };

        public class OurMap
        {
            public Dictionary<Position, string> mapPos;
            public Dictionary<Position, Position> portals;
            public Dictionary<Position, (string name, bool outer)> portalInfo;
            public OurMap()
            {
                mapPos = new Dictionary<Position, string>();
                portals = new Dictionary<Position, Position>();
                portalInfo = new Dictionary<Position, (string, bool)>();
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
                Map map = new Map(w, h, new Position(0, 0), '?');
                foreach (var kvp in mapPos)
                {
                    Position p = kvp.Key;
                    char c = kvp.Value[0];
                    map.data[p.x - x0, p.y - y0] = c;
                }
                map.Print();
            }
        }

        static List<string> ReadInput()
        {
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\..\input.txt");
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
            Dictionary<string, (Position, Position)> portals = new Dictionary<string, (Position, Position)>();
            Dictionary<(string, bool), Position> portalInfo = new Dictionary<(string, bool), Position>();
            void AddPortal(string s, Position p1, Position p2, bool outer)
            {
                Position p = p1;
                if (p.x < 0 || p.x >= w || p.y < 0 || p.y >= h || list[p.y][p.x] != '.')
                    p = p2;
                if (!portals.ContainsKey(s))
                    portals[s] = (p, new Position(-1, -1));
                else
                    portals[s] = (portals[s].Item1, p);
                portalInfo[(s, outer)] = p;
            }
            OurMap m = new OurMap();
            for (int y = 0; y < h; y++)
            {
                for (int x = 0; x < w; x++)
                {
                    char c = list[y][x];
                    if (c == '.')
                        m.mapPos[new Position(x, y)] = ".";
                    if (char.IsLetter(c) && x < w - 1 && y < h - 1)
                    {
                        char c2x = list[y][x + 1];
                        char c2y = list[y + 1][x];
                        if (char.IsLetter(c2x))
                            AddPortal(c.ToString() + c2x, new Position(x - 1, y), new Position(x + 2, y), x == 0 || x == w - 2);
                        if (char.IsLetter(c2y))
                            AddPortal(c.ToString() + c2y, new Position(x, y - 1), new Position(x, y + 2), y == 0 || y == h - 2);
                    }
                }
            }
            foreach (var v in portals)
            {
                if (v.Value.Item2.x >= 0)
                {
                    m.portals[v.Value.Item1] = v.Value.Item2;
                    m.portals[v.Value.Item2] = v.Value.Item1;
                }
            }
            foreach (var v in portalInfo)
            {
                m.portalInfo[v.Value] = v.Key;
            }
            return m;
        }

        static int FindShortestPath(OurMap m, int depthAdd)
        {
            Position startPos = m.portalInfo.Where(w => w.Value.name == "AA").First().Key;
            List<(Position pos, int level)> toVisit = new List<(Position, int)>() { (startPos, 0) };
            Dictionary<(Position pos, int level), int> steps = new Dictionary<(Position, int), int>() { { toVisit[0], 0 } };
            while (toVisit.Count > 0)
            {
                List<(Position pos, int level)> toVisitNext = new List<(Position, int)>();
                void TryGoTo((Position pos, int level) nextPos, (Position pos, int level) curPos)
                {
                    if (nextPos.level < 50 && nextPos.level >= 0)
                    {
                        if (!steps.ContainsKey(nextPos) || steps[nextPos] > steps[curPos] + 1)
                        {
                            toVisitNext.Add(nextPos);
                            steps[nextPos] = steps[curPos] + 1;
                        }
                    }
                }
                foreach (var v in toVisit)
                {
                    foreach (Position d in directions)
                    {
                        (Position pos, int level) nextPos = (v.pos + d, v.level);
                        if (m.mapPos.ContainsKey(nextPos.pos))
                            TryGoTo(nextPos, v);
                    }
                    if (m.portals.ContainsKey(v.pos))
                    {
                        int levelAdd = m.portalInfo[v.pos].outer ? -depthAdd : depthAdd;
                        (Position pos, int level) nextPos = (m.portals[v.pos], v.level + levelAdd);
                        TryGoTo(nextPos, v);
                    }
                }
                toVisit = toVisitNext;
            }
            Position endPos = m.portalInfo.Where(w => w.Value.name == "ZZ").First().Key;
            return steps[(endPos, 0)];
        }

        static void PartA()
        {
            List<string> input = ReadInput();
            OurMap m = BuildMap(input);
            int a = FindShortestPath(m, 0);
            Console.WriteLine("Part A: Result is {0}", a);
        }

        static void PartB()
        {
            List<string> input = ReadInput();
            OurMap m = BuildMap(input);
            int b = FindShortestPath(m, 1);
            Console.WriteLine("Part B: Result is {0}", b);
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2019 - " + typeof(Day20).Namespace + ":");
            PartA();
            PartB();
        }
    }
}
