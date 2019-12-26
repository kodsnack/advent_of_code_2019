using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

//using AdventOfCode;
//using Position = AdventOfCode.GenericPosition2D<int>;

namespace day22
{
    public class Day22
    {
        readonly static string nsname = typeof(Day22).Namespace;

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

        static void DealIntoNewStack(ref int[] deck1, ref int[] deck2)
        {
            for (int i = 0; i < cardsA; i++)
                deck2[cardsA - 1 - i] = deck1[i];
            int[] temp = deck1;
            deck1 = deck2;
            deck2 = temp;
        }

        static void DealWithIncrement(ref int[] deck1, ref int[] deck2, int n)
        {
            int a = 0;
            for (int i = 0; i < cardsA; i++)
            {
                deck2[a] = deck1[i];
                a += n;
                a %= cardsA;
            }
            int[] temp = deck1;
            deck1 = deck2;
            deck2 = temp;
        }

        static void CutDeckPositive(ref int[] deck1, ref int[] deck2, int n)
        {
            int i = 0;
            for (i = 0; i < n; i++)
                deck2[cardsA - n + i] = deck1[i];
            for (int a = 0; a < cardsA - n; a++)
                deck2[a] = deck1[i + a];
            int[] temp = deck1;
            deck1 = deck2;
            deck2 = temp;
        }

        static void CutDeckNegative(ref int[] deck1, ref int[] deck2, int n)
        {
            int i = 0;
            for (i = 0; i < n; i++)
                deck2[i] = deck1[cardsA - n + i];
            for (int a = 0; a < cardsA - n; a++)
                deck2[i + a] = deck1[a];
            int[] temp = deck1;
            deck1 = deck2;
            deck2 = temp;
        }

        static int[] ShuffleDeck(List<string> techniques, int[] deck1)
        {
            int[] deck2 = new int[cardsA];
            foreach (string s in techniques)
            {
                string[] v = s.Split(' ').ToArray();
                if (v[1] == "into")
                    DealIntoNewStack(ref deck1, ref deck2);
                else if (v[1] == "with")
                    DealWithIncrement(ref deck1, ref deck2, int.Parse(v[3]));
                else if (v[0] == "cut")
                {
                    int n = int.Parse(v[1]);
                    if (n > 0)
                        CutDeckPositive(ref deck1, ref deck2, n);
                    else if (n < 0)
                        CutDeckNegative(ref deck1, ref deck2, -n);
                }
                else throw new ArgumentOutOfRangeException();
            }
            return deck1;
        }

        const int cardsA = 10007;

        static Object PartA()
        {
            List<string> input = ReadInput();
            int[] deck1 = Enumerable.Range(0, cardsA).ToArray();
            deck1 = ShuffleDeck(input, deck1);
            int ans = deck1.ToList().IndexOf(2019);
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static long DeshuffleLargeDeck(List<string> techniques, long deckPosition)
        {
            long pos = deckPosition;
            foreach (string s in techniques)
            {
                string[] v = s.Split(' ').ToArray();
                if (v[1] == "into")
                {
                    pos = cardsB - 1 - pos;
                }
                else if (v[1] == "with")
                {
                    int n = int.Parse(v[3]);
                    checked
                    {
                        BigInteger bip = new BigInteger(pos);
                        BigInteger bim = new BigInteger(ModInverse(n, cardsB));
                        pos = (long) ((bip * bim) % new BigInteger(cardsB));
                    }
                }
                else if (v[0] == "cut")
                {
                    int n = int.Parse(v[1]);
                    if (n > 0)
                    {
                        long k = cardsB - n;
                        pos = (pos >= k) ? pos - k : pos + n;
                    }
                    else if (n < 0)
                    {
                        long ni = -n;
                        long k = cardsB - ni;
                        pos = (pos < ni) ? pos + k : pos - ni;
                    }
                }
                else throw new ArgumentOutOfRangeException();
            }
            return pos;
        }

        static long ModInverse(long a, long m)
        {
            if (m == 1) return 0;
            long m0 = m;
            (long x, long y) = (1, 0);

            while (a > 1)
            {
                long q = a / m;
                (a, m) = (m, a % m);
                (x, y) = (y, x - q * y);
            }
            return x < 0 ? x + m0 : x;
        }

        const long cardsB = 119315717514047;

        static Object PartB()
        {
            //int c = 19;
            //int b = 3;
            //for (int a = 0; a < c; a++)
            //{
            //    int x = (a * b) % c;
            //    int y = ((int)ModInverse(b, c) * x) % c;
            //    Console.WriteLine(
            //        "{0,3} * {1} % {2} == {3,3}. {4,3} {5}",
            //        a, b, c, x, y, (y == a ? "ok" : "FAIL"));
            //}
            const long iter = 101741582076661;
            List<string> input = ReadInput();
            List<string> flipped = new List<string>(input);
            flipped.Reverse();
            const long deckPosition = 2020;
            HashSet<long> visited = new HashSet<long>();
            long pos = deckPosition;
            long i = 0;
            while (!visited.Contains(pos))
            {
                visited.Add(pos);
                //Console.WriteLine("{0}", pos);
                pos = DeshuffleLargeDeck(flipped, pos);
                i++;
                if (i % 100000 == 0)
                    Console.Write(".");
            }
            Console.WriteLine();
            long ans = i;
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
            int a = 42;
            long b = 4711;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
