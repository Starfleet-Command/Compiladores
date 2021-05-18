/*
  Drac Semantic Visitor class

  Rubén Sánchez             A01021759
  Juan Francisco Gortarez   A01021926
  Martín Alegría            A01022216
*/

using System;
using System.Collections.Generic;

namespace Drac {

    public struct FuncProperties
    {
        public FuncProperties(bool p, int a, SortedDictionary<string, string> r)
        {
            primitive = p;
            arity = a;
            reference = r;
        }

        public bool primitive { get; set; }
        public int arity { get; set; }
        public SortedDictionary<string, string> reference { get; set; }

        public override string ToString() => $"({primitive}, {arity}, {reference})";
    }

    class SemanticVisitor {

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
            if (!FunTable.ContainsKey("main")) {
                    throw new SemanticError(
                        "No main function",
                        node.AnchorToken);
            }
        }

        //-----------------------------------------------------------
        public void Visit(DefinitionList node) {
            VisitChildren(node);
        }

        //-----------------------------------------------------------
        public void Visit(VarDef node) {
            Visit((dynamic) node[0]);
        }

        public void Visit(VarList node) {
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
                var arity=node[0].Size();
                FunTable[funcName] = new FuncProperties(false, arity, null);
            }
        }

        //-----------------------------------------------------------
        void VisitChildren(Node node) {
            foreach (var n in node) {
                Visit((dynamic) n);
            }
        }

    }

    class SemanticVisitor2 {

        public ISet<string> VarTable {
            get;
            private set;
        }

        public IDictionary<string, FuncProperties> FunTable {
            get;
            private set;
        }

        public int level{
            get;
            private set;
        }

        public string currentFunction{
            get;
            private set;
        }

        //-----------------------------------------------------------
        public SemanticVisitor2(ISet<string> var, IDictionary<string, FuncProperties> fun) {
            VarTable = var;
            FunTable = fun;
            level = 0;
            currentFunction = "";
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
            if(level>0){
                Visit((dynamic) node[0]);
            }
        }

        public void Visit(VarList node) {
            foreach (var n in node) {
                var variableName = n.AnchorToken.Lexeme;
                var props = FunTable[currentFunction];

                if (props.reference.ContainsKey(variableName)) {
                    throw new SemanticError(
                        "Duplicated variable: " + variableName,
                        n.AnchorToken);

                } else {
                    props.reference[variableName]="local";

                    FunTable[currentFunction] = props;
                }
            }
        }

        public void Visit(FunDef node) {

            var funcName = node.AnchorToken.Lexeme;

            var props = FunTable[funcName];

            currentFunction=funcName;
            level++;

            props.reference = new SortedDictionary<string, string>(); 

            FunTable[funcName] = props;
        
            Visit((dynamic) node[0]);
            
            Visit((dynamic) node[1]);

            /*
            foreach (var entry in FunTable[funcName].reference) {
                Console.WriteLine(entry);
            }
            Console.WriteLine("");
            */

            Visit((dynamic) node[2]);

            level--;
        }

        public void Visit(ParamList node) {
            foreach (var n in node) {
                var variableName = n.AnchorToken.Lexeme;

                var props = FunTable[currentFunction];

                props.reference[variableName]="param";

                FunTable[currentFunction] = props;
            }
        }

        public void Visit(VarDefList node) {
            VisitChildren(node);
        }

        public void Visit(StatementList node) {
            VisitChildren(node);
        }

        public void Visit(Assignment node) {
            var variableName = node.AnchorToken.Lexeme;
            var props = FunTable[currentFunction];

            if (props.reference.ContainsKey(variableName)) {
                
            }
            else if(VarTable.Contains(variableName)) {
                
            }
            else {
                throw new SemanticError(
                    "Undeclared variable: " + variableName,
                    node.AnchorToken);
            }

            Visit((dynamic) node[0]);

        }

        public void Visit(FuncCall node) {
            var funcName = node.AnchorToken.Lexeme;

            if (FunTable.ContainsKey(funcName)) {
                var props = FunTable[funcName];
                if(node[0].Size()==props.arity){
                    Visit((dynamic) node[0]);
                }
                else{
                    throw new SemanticError(
                        "Function call has incorrect number of arguments: " + funcName,
                        node.AnchorToken);
                }
            } else {
                throw new SemanticError(
                    "Undefined function: " + funcName,
                    node.AnchorToken);
            }
        }

        public void Visit(If node) {
            VisitChildren(node);
        }

        public void Visit(Inc node) {
            Visit((dynamic) node[0]);
        }

        public void Visit(Dec node) {
            Visit((dynamic) node[0]);
        }

        public void Visit(While node) {
            level++;
            Visit((dynamic) node[0]);
            Visit((dynamic) node[1]);
            level--;
        }

        public void Visit(DoWhile node) {
            level++;
            Visit((dynamic) node[0]);
            Visit((dynamic) node[1]);
            level--;
        }

        public void Visit(Return node) {
            
        }

        public void Visit(Break node) {
            if(level<2){
                throw new SemanticError(
                    "Break statement outside while/do-while",
                    node.AnchorToken);
            }
        }

        public void Visit(ExpressionList node) {
            VisitChildren(node);
        }

        public void Visit(Identifier node) {
            var variableName = node.AnchorToken.Lexeme;
            var props = FunTable[currentFunction];

            if (props.reference.ContainsKey(variableName)) {
                
            }
            else if(VarTable.Contains(variableName)) {
                
            }
            else {
                throw new SemanticError(
                    "Undeclared variable: " + variableName,
                    node.AnchorToken);
            }
        }

        public void Visit(String node) {
        }

        public void Visit(IntLiteral node) {
            var intStr = node.AnchorToken.Lexeme;
            int value;

            if (!Int32.TryParse(intStr, out value)) {
                throw new SemanticError(
                    $"Integer literal too large: {intStr}",
                    node.AnchorToken);
            }
        }

        public void Visit(ElseIfList node) {
            VisitChildren(node);
        }

        public void Visit(ElseIf node) {
            VisitChildren(node);
        }

        public void Visit(Else node) {
            VisitChildren(node);
        }
        
        public void Visit(True node) {
        }

        public void Visit(False node) {
        }

        public void Visit(Not node) {
            Visit((dynamic) node[0]);
        }
        
        public void Visit(And node) {
            Visit((dynamic) node[0]);
            Visit((dynamic) node[1]);
        }

        public void Visit(Or node) {
            Visit((dynamic) node[0]);
            Visit((dynamic) node[1]);
        }

        public void Visit(Less node) {
            Visit((dynamic) node[0]);
            Visit((dynamic) node[1]);
        }

        public void Visit(More node) {
            Visit((dynamic) node[0]);
            Visit((dynamic) node[1]);
        }

        public void Visit(LessEqual node) {
            Visit((dynamic) node[0]);
            Visit((dynamic) node[1]);
        }

        public void Visit(MoreEqual node) {
            Visit((dynamic) node[0]);
            Visit((dynamic) node[1]);
        }

        public void Visit(Add node) {
            Visit((dynamic) node[0]);
            Visit((dynamic) node[1]);
        }

        public void Visit(Subtr node) {
            Visit((dynamic) node[0]);
            Visit((dynamic) node[1]);
        }

        public void Visit(Mul node) {
            Visit((dynamic) node[0]);
            Visit((dynamic) node[1]);
        }

        public void Visit(Div node) {
            Visit((dynamic) node[0]);
            Visit((dynamic) node[1]);
        }

        public void Visit(Mod node) {
            Visit((dynamic) node[0]);
            Visit((dynamic) node[1]);
        }

        public void Visit(OpComp node) {
            Visit((dynamic) node[0]);
            Visit((dynamic) node[1]);
        }
        
        public void Visit(ExpressionUnary node) {
        }

        public void Visit(OpUnaryList node) {
        }

        public void Visit(EqualTo node) {
            Visit((dynamic) node[0]);
            Visit((dynamic) node[1]);
        }

        public void Visit(Char node) {
        }

        public void Visit(UnicodeChar node) {
        }

        public void Visit(Empty node) {
        }

        public void Visit(Array node) {
            Visit((dynamic) node[0]);
        }



        //-----------------------------------------------------------
        void VisitChildren(Node node) {
            foreach (var n in node) {
                Visit((dynamic) n);
            }
        }

    }
}
