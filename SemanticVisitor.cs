/*
  Drac Semantic Visitor class

  Rubén Sánchez             A01021759
  Juan Francisco Gortarez   A01021926
  Martín Alegría            A01022216
*/

using System;
using System.Collections.Generic;

namespace Drac {

    class SemanticVisitor {

        //-----------------------------------------------------------
        public struct FuncProperties
        {
            public FuncProperties(bool p, int a, string r)
            {
                primitive = p;
                arity = a;
                reference = r;
            }

            public bool primitive { get; set; }
            public int arity { get; set; }
            public string reference { get; set; }

            public override string ToString() => $"({primitive}, {arity}, {reference})";
        }

        public ISet<string> VarTable {
            get;
            private set;
        }

        public IDictionary<string, FuncProperties> FunTable {
            get;
            private set;
        }

        //-----------------------------------------------------------
        public SemanticVisitor() {
            VarTable = new HashSet<string>();
            FunTable = new SortedDictionary<string, FuncProperties>(){
                {"printi", new FuncProperties(true, 1, null)},
                {"printc", new FuncProperties(true, 1, null)},
                {"prints", new FuncProperties(true, 1, null)},
                {"println", new FuncProperties(true, 0, null)},
                {"readi", new FuncProperties(true, 0, null)},
                {"reads", new FuncProperties(true, 0, null)},
                {"new", new FuncProperties(true, 1, null)},
                {"size", new FuncProperties(true, 1, null)},
                {"add", new FuncProperties(true, 2, null)},
                {"get", new FuncProperties(true, 2, null)},
                {"set", new FuncProperties(true, 3, null)},
            };
        }

        //-----------------------------------------------------------
        public void Visit(Program node) {
            Visit((dynamic) node[0]);
        }

        //-----------------------------------------------------------
        public void Visit(DefinitionList node) {
            VisitChildren(node);
        }

        //-----------------------------------------------------------
        public void Visit(VarDef node) {
            Visit((dynamic) node[0]);
        }

        public void Visit(IdList node) {
            foreach (var n in node) {
                var variableName = n.AnchorToken.Lexeme;

                if (VarTable.Contains(variableName)) {
                    throw new SemanticError(
                        "Duplicated variable: " + variableName,
                        n.AnchorToken);

                } else {
                    VarTable.Add(variableName);
                }
            }
        }

        public void Visit(FunDef node) {

            var funcName = node.AnchorToken.Lexeme;

            if (FunTable.ContainsKey(funcName)) {
                throw new SemanticError(
                    "Duplicated function: " + funcName,
                    node.AnchorToken);

            } else {
                var arity=0;
                foreach (var n in node[0]) {
                    arity++;
                }
                FunTable[funcName] = new FuncProperties(false, arity, "x");
            }
        }

        //-----------------------------------------------------------
        void VisitChildren(Node node) {
            foreach (var n in node) {
                Visit((dynamic) n);
            }
        }

    }
}
