using System;

namespace day04
{
    class Day04
    {
        static bool TwoAdjSame(string s)
        {
            bool same = false;
            for (int i = 0; i < s.Length - 1 && !same; i++)
                same = s[i] == s[i + 1];
            return same;
        }

        static bool ExactlyTwoAdjSame(string s)
        {
            string t = "x" + s + "x";
            bool same = false;
            for (int i = 0; i < t.Length - 3 && !same; i++)
            {
                char a = t[i + 0];
                char b = t[i + 1];
                char c = t[i + 2];
                char d = t[i + 3];
                same = (a != b) && (b == c) && (c != d);
            }
            return same;
        }

        static bool NeverDecrease(string s)
        {
            for (int i = 0; i < s.Length - 1; i++)
            {
                if (s[i] > s[i + 1])
                    return false;
            }
            return true;
        }

        static string Increase(string s)
        {
            char[] a = s.ToCharArray();
            int i = a.Length - 1;
            bool overflow = false;
            do
            {
                overflow = (a[i] == '9');
                if (overflow)
                    a[i] = '0';
                else
                    a[i]++;
                i--;
            }
            while (overflow);
            return new string(a);
        }

        static readonly int inputMin = 137683;
        static readonly int inputMax = 596253;

        static void PartA()
        {
            int valid = 0;
            int i = inputMin;
            do
            {
                string s = i.ToString();
                if (TwoAdjSame(s) && NeverDecrease(s))
                    valid++;
                i++;
            }
            while (i <= inputMax);
            Console.WriteLine("Part A: Result is {0}.", valid);
        }

        static void PartB()
        {
            int valid = 0;
            int i = inputMin;
            do
            {
                string s = i.ToString();
                if (ExactlyTwoAdjSame(s) && NeverDecrease(s))
                    valid++;
                i++;
            }
            while (i <= inputMax);
            Console.WriteLine("Part B: Result is {0}.", valid);
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2019 - " + typeof(Day04).Namespace + ":");
            PartA();
            PartB();
        }
    }
}
