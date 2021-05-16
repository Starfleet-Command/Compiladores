/*
  Drac Compiler main driver class

  Rubén Sánchez             A01021759
  Juan Francisco Gortarez   A01021926
  Martín Alegría            A01022216
*/

using System;
using System.IO;
using System.Text;

namespace Drac {

    public class Driver {

        const string VERSION = "0.4";

        //-----------------------------------------------------------
        static readonly string[] ReleaseIncludes = {
            "Lexical analysis",
            "Syntactic analysis",
            "AST construction",
            "Semantic analysis"
        };

        //-----------------------------------------------------------
        void PrintAppHeader() {
            Console.WriteLine("Drac compiler, version " + VERSION);
            Console.WriteLine("This program is free software; you may "
                + "redistribute it under the terms of");
            Console.WriteLine("the GNU General Public License version 3 or "
                + "later.");
            Console.WriteLine("This program has absolutely no warranty.");
        }

        //-----------------------------------------------------------
        void PrintReleaseIncludes() {
            Console.WriteLine("Included in this release:");
            foreach (var phase in ReleaseIncludes) {
                Console.WriteLine("   * " + phase);
            }
        }

        //-----------------------------------------------------------
        void Run(string[] args) {

            PrintAppHeader();
            Console.WriteLine();
            PrintReleaseIncludes();
            Console.WriteLine();

            if (args.Length != 1) {
                Console.Error.WriteLine(
                    "Please specify the name of the input file.");
                Environment.Exit(1);
            }

            try {
                var inputPath = args[0];
                var input = File.ReadAllText(inputPath);
                /*
                Console.WriteLine(
                    $"===== Tokens from: \"{inputPath}\" =====");
                var count = 1;
                foreach (var tok in new Scanner(input).Scan()) {
                    Console.WriteLine($"[{count++}] {tok}");
                }
                */
                
                var parser = new Parser(
                    new Scanner(input).Scan().GetEnumerator());
                var program = parser.Program();
                Console.WriteLine("Syntax OK.");

                Console.Write(program.ToStringTree());

                
                var semantic = new SemanticVisitor();
                semantic.Visit((dynamic) program);
                
                Console.WriteLine("Semantics OK.");
                Console.WriteLine();
                Console.WriteLine("Variable Table");
                Console.WriteLine("============");
                foreach (var entry in semantic.VarTable) {
                    Console.WriteLine(entry);
                }
                Console.WriteLine();
                Console.WriteLine("Function Table");
                Console.WriteLine("============");
                foreach (var entry in semantic.FunTable) {
                    Console.WriteLine(entry);
                }
                
                

            } catch (Exception e) {

                if (e is FileNotFoundException || e is SyntaxError
                || e is SemanticError
                ) {
                    Console.Error.WriteLine(e.Message);
                    Environment.Exit(1);
                }

                throw;
            }
        }

        //-----------------------------------------------------------
        public static void Main(string[] args) {
            new Driver().Run(args);
        }
    }
}
