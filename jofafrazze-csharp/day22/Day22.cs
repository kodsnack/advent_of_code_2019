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

        // f(x) = (ax + b) mod m
        static void DeshuffleLargeDeck(List<string> techniques, long m, ref long a, ref long b)
        {
            foreach (string s in techniques)
            {
                string[] v = s.Split(' ').ToArray();
                if (v[1] == "into")
                {
                    a = -a;
                    b = m - b - 1;
                }
                else if (v[1] == "with")
                {
                    int n = int.Parse(v[3]);
                    checked
                    {
                        BigInteger mw = new BigInteger(m);
                        BigInteger mwi = new BigInteger(ModInverse(n, m));
                        a = (long)((a * mwi) % mw);
                        b = (long)((b * mwi) % mw);
                    }
                }
                else if (v[0] == "cut")
                {
                    int n = int.Parse(v[1]);
                    b = (b + n) % m;
                }
                else throw new ArgumentOutOfRangeException();
            }
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

        static bool TestModRoundtrip(int a, int b, int m, bool print = false)
        {
            // f(x) = (ax + b) % m
            // f'(y) = ((y + m - b) * ModInverse(a, m)) % m
            // f'(f(x)) = x
            bool ok = true;
            for (int x = 0; x < m; x++)
            {
                int fx = ((a * x) + b) % m;
                int fy = ((fx + m - b) * (int)ModInverse(a, m)) % m;
                ok &= x == fy;
                if (print)
                {
                    Console.WriteLine(
                        "f(x) = (({0} * {1,2}) + {2}) % {3} = {4,2}. " +
                        "f'(y) = (({4,2} + {3} - {2}) * ModInv({0}, {3}) % {3} = {5,2}. " +
                        "[{1,2} == {5,2} ? {6}]",
                        a, x, b, m, fx, fy, (fy == x ? "ok" : "FAIL"));
                }
            }
            return ok;
        }

        static void TestModInverse(int mMax, bool print)
        {
            //TestModRoundtrip(13, 7, 23, print);
            bool ok = true;
            for (int m = 2; m < mMax; m++)
            {
                for (int a = 1; a < m; a++)
                {
                    if (GCF(a, m) == 1)
                        for (int b = 0; b < m; b++)
                            ok &= TestModRoundtrip(a, b, m);
                }
            }
            if (print)
                Console.WriteLine("TestModInverse: {0}", ok ? "Passed" : "FAILED");
        }

        static long Normalize(long a, long m)
        {
            return (a + m) % m;
        }

        static (long, long) CalculateModularExponentiation(long a, long b, long e, long m)
        {
            long an = (long)BigInteger.ModPow(a, e, m);
            BigInteger bw = new BigInteger(b);
            long bn = (long)((bw * (1 - an) * ModInverse(Normalize(1 - a, m), m)) % m);
            return (Normalize(an, m), Normalize(bn, m));
        }

        static Object PartB()
        {
            // f(x) = (ax + b) % m
            // f'(y) = (y * ModInverse(b, m)) % m
            //TestModInverse(50, true);
            const long cardsB = 119315717514047;
            const long iter = 101741582076661;
            List<string> input = ReadInput();
            List<string> flipped = new List<string>(input);
            flipped.Reverse();
            const long deckPosition = 2020;
            long a = 1;
            long b = 0;
            DeshuffleLargeDeck(flipped, cardsB, ref a, ref b);
            //DeshuffleLargeDeck(flipped, cardsA, ref a, ref b);
            //long test = (a * 8502 + b) % cardsA + cardsA;
            (long an, long bn) = CalculateModularExponentiation(a, b, iter, cardsB);
            long ans = Normalize(((an * deckPosition) + bn), cardsB);
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
            int a = 8502;
            long b = 41685581334351;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
