using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;

namespace day16
{
    class Day16
    {
        static List<int> ReadInput()
        {
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\..\input.txt");
            StreamReader reader = File.OpenText(path);
            List<int> list = new List<int>();
            string line = reader.ReadLine();
            foreach (char c in line)
            {
                list.Add(int.Parse(c.ToString()));
            }
            return list;
        }

        static int[] DoFFTPhase(int[] numbers)
        {
            int s = numbers.Length;
            int[] res = new int[s];
            for (int i = 0; i < s; i++)
            {
                long sum = 0;
                for (int b = 0; b < s; b++)
                {
                    int k = ((b + 1) / (i + 1)) % 4;
                    if (k == 1)
                        sum += numbers[b % s];
                    else if (k == 3)
                        sum -= numbers[b % s];
                }
                res[i] = (int) Math.Abs(sum) % 10;
            }
            return res;
        }

        static int[] DoFFTB(int[] numbers, int offs)
        {
            int s = numbers.Length;
            int large = s * 10000;
            if ((large / 2) < offs && offs <= large - 8)
            {
                Console.WriteLine("{0} / 2 < {1} <= {2} - 8, OK!", large, offs, large);
            }
            else
            {
                throw new ArgumentOutOfRangeException();
            }
            int[] fftNums = new int[large - offs];
            for (int i = 0; i < fftNums.Length; i++)
                fftNums[i] = numbers[(offs + i) % s];
            int[] fftNumsNext = new int[fftNums.Length];
            for (int iter = 0; iter < 100; iter++)
            {
                int sum = 0;
                for (int i = 0; i < fftNums.Length; i++)
                    sum += fftNums[i];
                fftNumsNext[0] = sum % 10;
                for (int i = 1; i < fftNumsNext.Length; i++)
                    fftNumsNext[i] = (fftNumsNext[i - 1] - fftNums[i - 1] + 10) % 10;
                fftNumsNext.CopyTo(fftNums, 0);
            }
            return fftNums.Take(8).ToArray();
        }

        static void PartA()
        {
            List<int> input = ReadInput();
            int[] numbers = input.ToArray();
            for (int i = 0; i < 100; i++)
            {
                numbers = DoFFTPhase(numbers);
            }
            string ans = "";
            for (int i = 0; i < 8; i++)
                ans += numbers[i].ToString();
            Console.WriteLine("Part A: Result is {0}", ans);
        }

        static void PartB()
        {
            List<int> input = ReadInput();
            int[] numbers = input.ToArray();
            string offs = new string(numbers.Take(7).Select(x => x.ToString()[0]).ToArray());
            int[] res = DoFFTB(numbers, int.Parse(offs));
            string ans = "";
            for (int i = 0; i < 8; i++)
                ans += res[i].ToString();
            Console.WriteLine("Part B: Result is {0}", ans);
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2019 - " + typeof(Day16).Namespace + ":");
            PartA();
            PartB();
        }
    }
}
