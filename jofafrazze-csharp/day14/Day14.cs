using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace day14
{
    public class Day14
    {
        readonly static string nsname = typeof(Day14).Namespace;

        public struct Material
        {
            public string name;
            public int amount;
            public Material(string n, int a) { name = n; amount = a; }
        }

        public struct Reaction
        {
            public Material product;
            public List<Material> ingredients;
            public Reaction(Material p) { product = p; ingredients = new List<Material>(); }
        }

        static Dictionary<string, Reaction> ReadInput()
        {
            string path = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");
            StreamReader reader = File.OpenText(path);
            Dictionary<string, Reaction> reactions = new Dictionary<string, Reaction>();
            Material GetMaterial(string s)
            {
                string[] v = s.Split(" ", StringSplitOptions.RemoveEmptyEntries).ToArray();
                return new Material(v[1], int.Parse(v[0]));
            }
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                string[] v = line.Split("=>").ToArray();
                Reaction r = new Reaction(GetMaterial(v[1]));
                string[] z = v[0].Split(',', StringSplitOptions.RemoveEmptyEntries).ToArray();
                foreach (string s in z)
                {
                    r.ingredients.Add(GetMaterial(s));
                }
                reactions[r.product.name] = r;
            }
            return reactions;
        }

        static void AddChemical(ref Dictionary<string, long> dict, string n, long a)
        {
            if (!dict.ContainsKey(n))
                dict[n] = 0;
            dict[n] += a;
        }

        static void PrintMaterials(Dictionary<string, long> materials)
        {
            foreach (var kvp in materials)
            {
                Console.Write("{0} {1}, ", kvp.Value, kvp.Key);
            }
            Console.WriteLine();
        }

        static Dictionary<string, long> BreakDownChemicals(Dictionary<string, Reaction> reactions, string cName, long cAmount)
        {
            Dictionary<string, long> chemicals = new Dictionary<string, long>();
            chemicals[cName] = cAmount;
            void DissolveChemical(ref Dictionary<string, long> materials, string name, long a)
            {
                Reaction r = reactions[name];
                int w = r.product.amount;
                long n = ((a - 1) / w) + 1;
                if (n > 0)
                {
                    AddChemical(ref materials, r.product.name, -n * w);
                    foreach (Material m in r.ingredients)
                        AddChemical(ref materials, m.name, m.amount * n);
                }
            }
            int nChemicals = 0;
            do
            {
                //PrintMaterials(chemicals);
                Dictionary<string, long> materials = new Dictionary<string, long>(chemicals);
                foreach (var kvp in chemicals)
                {
                    if (kvp.Key != "ORE" && kvp.Value > 0)
                    {
                        DissolveChemical(ref materials, kvp.Key, kvp.Value);
                    }
                }
                chemicals = materials;
                nChemicals = chemicals.Where(x => x.Value > 0).Count();
            }
            while (!chemicals.ContainsKey("ORE") || nChemicals > 1);
            return chemicals;
        }
        static bool PartA(Object correctAnswer = null)
        {
            Dictionary<string, Reaction> input = ReadInput();
            Dictionary<string, long> chemicals = BreakDownChemicals(input, "FUEL", 1);
            long res = chemicals["ORE"];
            Console.WriteLine("Part A: Result is {0}", res);
            return correctAnswer == null || res == (long)correctAnswer;
        }

        static bool PartB(Object correctAnswer = null)
        {
            Dictionary<string, Reaction> input = ReadInput();
            Dictionary<string, long> chemicals = BreakDownChemicals(input, "FUEL", 1);
            const long nOre = 1000000000000;
            long orePerFuel = chemicals["ORE"];
            long start = nOre / orePerFuel;
            long maxFuel = 0;
            bool done = false;
            for (long i = 1000; !done; i = (i * 120) / 100)
            {
                chemicals = BreakDownChemicals(input, "FUEL", start + i);
                done = chemicals["ORE"] > nOre;
                if (!done)
                    maxFuel = start + i;
            }
            done = false;
            long start2 = maxFuel;
            for (long i = 0; !done; i++)
            {
                chemicals = BreakDownChemicals(input, "FUEL", start2 + i);
                done = chemicals["ORE"] > nOre;
                if (!done)
                    maxFuel = start2 + i;
            }
            Console.WriteLine("Part B: Result is {0}", maxFuel);
            return correctAnswer == null || maxFuel == (long)correctAnswer;
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2019 - " + nsname + ":");
            PartA();
            PartB();
        }

        public static bool MainTest()
        {
            long a = 1967319;
            long b = 1122036;
            return PartA(a) && PartB(b);
        }
    }
}
