﻿/*
  Drac compiler - WatVisitor class.

  Rubén Sánchez             A01021759
  Juan Francisco Gortarez   A01021926
  Martín Alegría            A01022216
*/

using System;
using System.Text;
using System.Collections.Generic;

namespace Drac {

    class WatVisitor {

        public ISet<string> VarTable {
            get;
            private set;
        }

        public IDictionary<string, FuncProperties> FunTable {
            get;
            private set;
        }

        public string currentFunction{
            get;
            private set;
        }

        public int labelCounter{
            get;
            private set;
        }

        public int hasReturn{
            get;
            private set;
        }

        public int level{
            get;
            private set;
        }

        public int levelIf{
            get;
            private set;
        }

        public WatVisitor(ISet<string> var, IDictionary<string, FuncProperties> fun) {
            VarTable = var;
            FunTable = fun;
            currentFunction = "";
            labelCounter = 0;
            hasReturn = 0;
            level=0;
        }

        public string GenerateLabel() {
            return $"${labelCounter++:00000}";
        }

        public string Visit(Program node) {
            return ";; WebAssembly text format code generated by "
                + "the buttercup compiler.\n\n"
                + "(module\n"
                + "  (import \"drac\" \"printi\" (func $printi (param i32) (result i32)))\n"
                + "  (import \"drac\" \"println\" (func $println (result i32)))\n"
                + "  (import \"drac\" \"readi\" (func $readi (result i32)))\n"
                + "  (import \"drac\" \"reads\" (func $reads (result i32)))\n"
                + "  (import \"drac\" \"size\" (func $size (param i32) (result i32)))\n"
                + "  (import \"drac\" \"get\" (func $get (param i32 i32) (result i32)))\n"
                + "  (import \"drac\" \"printc\" (func $printc (param i32) (result i32)))\n"
                + "  (import \"drac\" \"prints\" (func $prints (param i32) (result i32)))\n"
                + "  (import \"drac\" \"new\" (func $new (param i32) (result i32)))\n"
                + "  (import \"drac\" \"add\" (func $add (param i32 i32) (result i32)))\n"
                + "  (import \"drac\" \"set\" (func $set (param i32 i32 i32) (result i32)))\n"
                + Visit((dynamic) node[0])
                + ")\n";
        }

        public string Visit(DefinitionList node) {
            return VisitChildren(node);
        }

        
        public string Visit(VarDef node) {
            return VisitChildren(node);
        }

        public string Visit(VarList node) {
            var sb = new StringBuilder();
            if(currentFunction==""){
                foreach (var n in node) {
                    var variableName = n.AnchorToken.Lexeme;

                    sb.Append($"  (global ${variableName} (mut i32) (i32.const 0))\n");
                }
                return sb.ToString();
            }
            
            foreach (var n in node) {
                var variableName = n.AnchorToken.Lexeme;

                sb.Append($"    (local ${variableName} i32)\n");
            }

            return sb.ToString();
        }

        public string Visit(FunDef node) {
            var funcName = node.AnchorToken.Lexeme;
            currentFunction = funcName;
            var sb = new StringBuilder();

            if(funcName=="main"){
                sb.Append($"  (func\n");
                sb.Append($"    (export \"main\")\n");
            }
            else{
                sb.Append($"  (func ${funcName}\n");
            }

            sb.Append(Visit((dynamic) node[0]));
            sb.Append($"    (result i32)\n");
            sb.Append($"    (local $_temp i32)\n");
            sb.Append(Visit((dynamic) node[1]));
            sb.Append(Visit((dynamic) node[2]));
            if(hasReturn==0){
                sb.Append($"    i32.const 0\n");  
            }
            sb.Append($"  )\n");

            hasReturn = 0;

            return sb.ToString();
        }

        public string Visit(ParamList node) {
            var sb = new StringBuilder();
            
            foreach (var n in node) {
                var paramName = n.AnchorToken.Lexeme;

                sb.Append($"    (param ${paramName} i32)\n");
            }

            return sb.ToString();
        }

        public string Visit(VarDefList node) {
            return VisitChildren(node);
        }

        public string Visit(StatementList node) {
            return VisitChildren(node);
        }

        public string Visit(FuncCall node) {
            var sb = new StringBuilder();
            var funcName = node.AnchorToken.Lexeme;

            level+=1;

            sb.Append(Visit((dynamic) node[0]));
            sb.Append($"    call ${funcName}\n");

            level-=1;

            if(!(funcName=="reads" || funcName=="readi" || funcName=="new" || funcName=="size" || funcName=="get") && level==0){
                sb.Append($"    drop\n");
            }

            return sb.ToString();
        }

        public string Visit(Assignment node) {
            var variableName = node.AnchorToken.Lexeme;
            var props = FunTable[currentFunction];
            var sb = new StringBuilder();

            level+=1;

            if(props.reference.ContainsKey(variableName)){
                sb.Append(Visit((dynamic) node[0]));
                sb.Append($"    local.set ${variableName}\n");

                level-=1;
                return sb.ToString();
            }
            sb.Append(Visit((dynamic) node[0]));
            sb.Append($"    global.set ${variableName}\n");

            level-=1;
            return sb.ToString();
        }

        public string Visit(Inc node) {
            return Visit((dynamic) node[0])
                + $"    i32.const 1\n"
                + $"    i32.add\n"
                + $"    local.set ${node[0].AnchorToken.Lexeme}\n";
        }

        public string Visit(Dec node) {
            return Visit((dynamic) node[0])
                + $"    i32.const 1\n"
                + $"    i32.sub\n"
                + $"    local.set ${node[0].AnchorToken.Lexeme}\n";
        }
        
        public string Visit(Return node) {
            var sb = new StringBuilder();

            if(levelIf==0){
                hasReturn=1;
            }

            level++;
            sb.Append(Visit((dynamic) node[0]));
            level--;

            sb.Append($"    return\n");

            return sb.ToString();
        }

        public string Visit(Break node) {
            return $"    br 0\n";
        }
        
        public string Visit(ExpressionList node) {
            return VisitChildren(node);
        }




        public string Visit(String node) {
            var str = node.AnchorToken.Lexeme;
            var codePoints = new CodePoints();
            var laLista = codePoints.AsCodePoints(str);
            var sb = new StringBuilder();

            sb.Append($"    i32.const 0\n");
            sb.Append($"    call $new\n");
            sb.Append($"    local.set $_temp\n");
            sb.Append($"    local.get $_temp\n");

            if(str==""){
                return sb.ToString();
            }

            foreach(var letter in laLista){
                sb.Append($"    local.get $_temp\n");
            }

            foreach(var letter in laLista){
                sb.Append($"    i32.const {letter}\n");
                sb.Append($"    call $add\n");
                sb.Append($"    drop\n");
            }

            return sb.ToString();
        }

        public string Visit(Char node) {
            var chr = node.AnchorToken.Lexeme;
            var codePoints = new CodePoints();
            var laLista = codePoints.AsCodePoints(chr);
            var sb = new StringBuilder();

            sb.Append($"    i32.const {laLista[0]}\n");

            return sb.ToString();
        }

        public string Visit(IntLiteral node) {
            return $"    i32.const {node.AnchorToken.Lexeme}\n";
        }

        
        public string Visit(Identifier node) {
            return $"    local.get ${node.AnchorToken.Lexeme}\n";
        }
        
        public string Visit(If node) {
            var sb = new StringBuilder();
            levelIf++;
            level++;
            sb.Append(Visit((dynamic) node[0]));
            level--;
            sb.Append("    if\n");
            sb.Append(Visit((dynamic) node[1]));
            sb.Append(Visit((dynamic) node[2]));
            sb.Append(Visit((dynamic) node[3]));
            sb.Append("    end\n");
            levelIf--;

            return sb.ToString();
        }

        public string Visit(ElseIfList node) {
            return VisitChildren(node);
        }

        public string Visit(ElseIf node) {
            return "    else\n"
                + Visit((dynamic) node[0])
                + "    if\n"
                + Visit((dynamic) node[1])
                + "    end\n";
        }

        public string Visit(Else node) {
            if(node.Size()==1){
                return "    else\n"
                + Visit((dynamic) node[0]);
            }
            return "";
        }

        public string Visit(While node) {
            var sb = new StringBuilder();
            var labelBlock = GenerateLabel();
            var labelLoop = GenerateLabel();

            sb.Append($"    block {labelBlock}\n");
            sb.Append($"    loop {labelLoop}\n");
            level++;
            sb.Append(Visit((dynamic) node[0]));
            level--;
            sb.Append($"    i32.eqz\n");
            sb.Append($"    br_if {labelBlock}\n");
            sb.Append(Visit((dynamic) node[1]));
            sb.Append($"    br {labelLoop}\n");
            sb.Append($"    end\n");
            sb.Append($"    end\n");

            return sb.ToString();
        }

        public string Visit(DoWhile node) {
            var sb = new StringBuilder();
            var labelBlock = GenerateLabel();
            var labelLoop = GenerateLabel();

            sb.Append($"    block {labelBlock}\n");
            sb.Append($"    loop {labelLoop}\n");
            sb.Append(Visit((dynamic) node[0]));
            level++;
            sb.Append(Visit((dynamic) node[1]));
            level--;
            sb.Append($"    i32.eqz\n");
            sb.Append($"    br_if {labelBlock}\n");
            sb.Append($"    br {labelLoop}\n");
            sb.Append($"    end\n");
            sb.Append($"    end\n");

            return sb.ToString();
        }

        public string Visit(ExpressionUnary node) {
            var sb = new StringBuilder();

            sb.Append(Visit((dynamic) node[1]));
            sb.Append(Visit((dynamic) node[0]));

            return sb.ToString();
        }

        public string Visit(OpUnaryList node) {
            return VisitChildren(node);
        }


        public string Visit(True node) {
            return "    i32.const 1\n";
        }

        public string Visit(False node) {
            return "    i32.const 0\n";
        }

        public string Visit(Not node) {
            return "    i32.eqz\n";
        }
        
        public string Visit(And node) {
            var sb = new StringBuilder();

            sb.Append(Visit((dynamic) node[0]));
            sb.Append($"    if (result i32)\n");
            sb.Append(Visit((dynamic) node[1]));
            sb.Append($"    i32.eqz\n");
            sb.Append($"    i32.eqz\n");
            sb.Append($"    else\n");
            sb.Append($"    i32.const 0\n");
            sb.Append($"    end\n");

            return sb.ToString();
        }

        public string Visit(Or node) {
            var sb = new StringBuilder();

            sb.Append(Visit((dynamic) node[0]));
            sb.Append($"    if (result i32)\n");
            sb.Append($"    i32.const 1\n");
            sb.Append($"    else\n");
            sb.Append(Visit((dynamic) node[1]));
            sb.Append($"    i32.eqz\n");
            sb.Append($"    i32.eqz\n");
            sb.Append($"    end\n");

            return sb.ToString();
        }

        public string Visit(Less node) {
            return Visit((dynamic) node[0])
                + Visit((dynamic) node[1])
                + $"    i32.lt_s\n";
        }

        public string Visit(More node) {
            return Visit((dynamic) node[0])
                + Visit((dynamic) node[1])
                + $"    i32.gt_s\n";
        }

        public string Visit(LessEqual node) {
            return Visit((dynamic) node[0])
                + Visit((dynamic) node[1])
                + $"    i32.le_s\n";
        }

        public string Visit(MoreEqual node) {
            return Visit((dynamic) node[0])
                + Visit((dynamic) node[1])
                + $"    i32.ge_s\n";
        }

        public string Visit(Add node) {
            return Visit((dynamic) node[0])
                + Visit((dynamic) node[1])
                + $"    i32.add\n";
        }

        public string Visit(Subtr node) {
            return Visit((dynamic) node[0])
                + Visit((dynamic) node[1])
                + $"    i32.sub\n";
        }

        public string Visit(Mul node) {
            return Visit((dynamic) node[0])
                + Visit((dynamic) node[1])
                + $"    i32.mul\n";
        }

        public string Visit(Div node) {
            return Visit((dynamic) node[0])
                + Visit((dynamic) node[1])
                + $"    i32.div_s\n";
        }

        public string Visit(Mod node) {
            return Visit((dynamic) node[0])
                + Visit((dynamic) node[1])
                + $"    i32.rem_s\n";
        }

        public string Visit(EqualTo node) {
            return Visit((dynamic) node[0])
                + Visit((dynamic) node[1])
                + $"    i32.eq\n";
        }

        public string Visit(OpComp node) {
            return Visit((dynamic) node[0])
                + Visit((dynamic) node[1])
                + $"    i32.ne\n";
        }

        /*
        //-----------------------------------------------------------
        public string Visit(DeclarationList node) {
            // The code for the local variable declarations is
            // generated directly from the symbol table, not from
            // the AST nodes.
            var sb = new StringBuilder();
            foreach (var entry in table) {
                sb.Append($"    (local ${entry.Key} i32)\n");
            }
            return sb.ToString();
        }

        //-----------------------------------------------------------
        public string Visit(StatementList node) {
            return VisitChildren(node);
        }

        //-----------------------------------------------------------
        public string Visit(Assignment node) {
            return Visit((dynamic) node[0])
                + $"    local.set ${node.AnchorToken.Lexeme}\n";
        }

        //-----------------------------------------------------------
        public string Visit(Print node) {
            return Visit((dynamic) node[0])
                + $"    call ${PrintInstructions[node.ExpressionType]}\n";
        }

        //-----------------------------------------------------------
        public string Visit(If node) {
            return Visit((dynamic) node[0])
                + "    if\n"
                + Visit((dynamic) node[1])
                + "    end\n";
        }

        //-----------------------------------------------------------
        public string Visit(Identifier node) {
            return $"    local.get ${node.AnchorToken.Lexeme}\n";
        }

        //-----------------------------------------------------------
        public string Visit(IntLiteral node) {
            return $"    i32.const {node.AnchorToken.Lexeme}\n";
        }

        //-----------------------------------------------------------
        public string Visit(True node) {
            return "    i32.const 1\n";
        }

        //-----------------------------------------------------------
        public string Visit(False node) {
            return "    i32.const 0\n";
        }

        //-----------------------------------------------------------
        public string Visit(Neg node) {
            return "    i32.const 0\n"
                + Visit((dynamic) node[0])
                + "    i32.sub\n";
        }

        //-----------------------------------------------------------
        public string Visit(And node) {
            return VisitBinaryOperator("i32.and", node);
        }

        //-----------------------------------------------------------
        public string Visit(Less node) {
            return VisitBinaryOperator("i32.lt_s", node);
        }

        //-----------------------------------------------------------
        public string Visit(Plus node) {
            return VisitBinaryOperator("i32.add", node);
        }

        //-----------------------------------------------------------
        public string Visit(Mul node) {
            return VisitBinaryOperator("i32.mul", node);
        }

        //-----------------------------------------------------------
        string VisitChildren(Node node) {
            var sb = new StringBuilder();
            foreach (var n in node) {
                sb.Append(Visit((dynamic) n));
            }
            return sb.ToString();
        }

        //-----------------------------------------------------------
        string VisitBinaryOperator(string op, Node node) {
            return Visit((dynamic) node[0])
                + Visit((dynamic) node[1])
                + $"    {op}\n";
        }
        */

        string VisitChildren(Node node) {
            var sb = new StringBuilder();
            foreach (var n in node) {
                sb.Append(Visit((dynamic) n));
            }
            return sb.ToString();
        }
    }
}