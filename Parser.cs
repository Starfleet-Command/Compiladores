/*
  Drac Compiler scanner class

  Rubén Sánchez             A01021759
  Juan Francisco Gortarez   A01021926
  Martín Alegría            A01022216
*/

/*
 * Buttercup LL(1) Grammar:
 *
 *      Program             ::=  Declaration* Statement* EOF
 *      Declaration         ::=  Type Identifier
 *      Type                ::=  "int" | "bool"
 *      Statement           ::=  Assignment | Print | If
 *      Assignment          ::=  Identifier "=" Expression
 *      Print               ::=  "print" Expression
 *      If                  ::=  "if" Expression "then" Statement* "end"
 *      Expression          ::=  SimpleExpression (Operator SimpleExpression)*
 *      Operator›           ::=  "&" | "<" | "+" | "*"
 *      SimpleExpression    ::=  Identifier | IntLiteral | "#t" | "#f"
 *                               | "(" Expression ")" | "-" SimpleExpression
 *
 *  Drac LL(1) Grammar:
 *  
 *  Program       ::=  Def*
 *  Def             ::= Var-Def | Fun-Def
 *  Var-Def         ::= "var" Var-List ";"
 *  Var-List      ::= ‹id-list›
 *  ‹id-list›       ::= <id> ("," <id>)*
 *  Fun-Def         ::= <id> "(" ‹param-list› ")" "{" ‹var-def-list› ‹stmt-list› "}"
 *  ‹param-list›    ::= ‹id-list›?
 *  ‹var-def-list›  ::= <var-def>*
 *  ‹stmt-list›     ::= ‹stmt›*
 *  ‹stmt›          ::= ‹stmt-assign› | ‹stmt-incr› | ‹stmt-decr› | ‹stmt-fun-call› | ‹stmt-if› | ‹stmt-while› | ‹stmt-do-while› | ‹stmt-break› | ‹stmt-return› | ‹stmt-empty›
 *  ‹stmt-assign›   ::= <id> "=" ‹expr› ";"
 *  ‹stmt-incr›     ::= "inc" <id> ";"
 *  ‹stmt-decr›     ::= "dec" <id> ";"
 *  ‹stmt-fun-call› ::= ‹fun-call› ";"
 *  ‹fun-call›      ::= <id> "(" ‹expr-list› ")"
 *  ‹expr-list›     ::= (<expr> ("," <expr>)*)?
 *  ‹stmt-if›       ::= "if" "(" ‹expr› ")" "{" ‹stmt-list› "}" ‹else-if-list› ‹else›
 *  ‹else-if-list›  ::= ("elif" "(" ‹expr› ")" "{" ‹stmt-list› "}")*
 *  ‹else›          ::= ("else" "{" ‹stmt-list› "}")?
 *  ‹stmt-while›    ::= "while" "(" ‹expr› ")" "{" ‹stmt-list› "}"
 *  ‹stmt-do-while› ::= "do" "{" ‹stmt-list› "}" "while" "(" ‹expr› ")" ";"
 *  ‹stmt-break›    ::= "break" ";"
 *  ‹stmt-return›   ::=	"return" ‹expr› ";"
 *  ‹stmt-empty›    ::= ";"
 *  ‹expr›          ::= ‹expr-or›
 *  ‹expr-or›       ::= ‹expr-and› ("or" ‹expr-and›)*
 *  ‹expr-and›      ::= ‹expr-comp› ("and" ‹expr-comp›)*
 *  ‹expr-comp›     ::= ‹expr-rel› (‹op-comp› ‹expr-rel›)*
 *  ‹op-comp›       ::= "==" | "<>"
 *  ‹expr-rel›      ::= ‹expr-add› (‹op-rel› ‹expr-add›)*
 *  ‹op-rel›        ::= "<" | ">" | "<=" | ">="
 *  ‹expr-add›      ::= ‹expr-mul› (‹op-add› ‹expr-mul›)*
 *  ‹op-add›        ::= "+" | "-"
 *  ‹expr-mul›      ::= ‹expr-unary› (‹op-mul› ‹expr-unary›)*
 *  ‹op-mul›        ::= "*" | "/" | "%"
 *  ‹expr-unary›    ::= ‹op-unary›* ‹expr-primary›
 *  ‹op-unary›      ::= "+" | "-" | "not"
 *  ‹expr-primary›  ::= ‹id› | ‹fun-call› | ‹array› | ‹lit› | "(" ‹expr› ")"
 *  ‹array›         ::= "[" ‹expr-list› "]"
 *  ‹lit›           ::= ‹lit-bool› | ‹lit-int› | ‹lit-char› | ‹lit-str›
 */

using System;
using System.Collections.Generic;

namespace Drac {

    class Parser {

        static readonly ISet<TokenCategory> firstOfDefinition =
            new HashSet<TokenCategory>() {
                TokenCategory.VAR,
                TokenCategory.IDENTIFIER
            };

        static readonly ISet<TokenCategory> firstOfStatement =
            new HashSet<TokenCategory>() {
                TokenCategory.IDENTIFIER,
                TokenCategory.INC,
                TokenCategory.DEC,
                TokenCategory.IF,
                TokenCategory.WHILE,
                TokenCategory.DO,
                TokenCategory.BREAK,
                TokenCategory.RETURN,
                TokenCategory.SEMICOLON
            };
        
        static readonly ISet<TokenCategory> secondOfStatement =
            new HashSet<TokenCategory>() {
                TokenCategory.ASSIGN,
                TokenCategory.SEMICOLON
            };

        static readonly ISet<TokenCategory> firstOfOperator =
            new HashSet<TokenCategory>() {
                TokenCategory.EQUAL_TO,
                TokenCategory.OP_COMP
            };

        static readonly ISet<TokenCategory> firstOfOperatorRel =
            new HashSet<TokenCategory>() {
                TokenCategory.MORE,
                TokenCategory.MORE_EQUAL,
                TokenCategory.LESS,
                TokenCategory.LESS_EQUAL
            };

        static readonly ISet<TokenCategory> firstOfOperatorAdd =
            new HashSet<TokenCategory>() {
                TokenCategory.ADD,
                TokenCategory.SUBTR
            };

        static readonly ISet<TokenCategory> firstOfOperatorMul =
            new HashSet<TokenCategory>() {
                TokenCategory.MUL,
                TokenCategory.DIV,
                TokenCategory.MOD
            };

        static readonly ISet<TokenCategory> firstOfOperatorUnary =
            new HashSet<TokenCategory>() {
                TokenCategory.ADD,
                TokenCategory.SUBTR,
                TokenCategory.NOT
            };
        
        static readonly ISet<TokenCategory> firstOfExpression =
            new HashSet<TokenCategory>() {
                TokenCategory.IDENTIFIER,
                TokenCategory.INT_LITERAL,
                TokenCategory.STRING,
                TokenCategory.CHAR,
                TokenCategory.UNICODE_CHAR,
                TokenCategory.TRUE,
                TokenCategory.FALSE,
                TokenCategory.SQUARE_OPEN,
                TokenCategory.PARENTHESIS_OPEN,
                TokenCategory.ADD,
                TokenCategory.SUBTR,
                TokenCategory.NOT
            };

        IEnumerator<Token> tokenStream;

        public Parser(IEnumerator<Token> tokenStream) {
            this.tokenStream = tokenStream;
            this.tokenStream.MoveNext();
        }

        public TokenCategory CurrentToken {
            get { return tokenStream.Current.Category; }
        }

        public Token Expect(TokenCategory category) {
            if (CurrentToken == category) {
                Token current = tokenStream.Current;
                tokenStream.MoveNext();
                return current;
            } else {
                throw new SyntaxError(category, tokenStream.Current);
            }
        }

        public Node Program() {

            var defList = new DefinitionList();

            while (firstOfDefinition.Contains(CurrentToken)) {
                defList.Add(Definition());
            }

            var result = new Program(){ defList };
            result.AnchorToken = Expect(TokenCategory.EOF);

            return result;
        }

        public Node Definition() {

            switch (CurrentToken) {

            case TokenCategory.VAR:
                return VarDef();

            case TokenCategory.IDENTIFIER:
               return FunDef();

            default:
                throw new SyntaxError(firstOfDefinition,
                                      tokenStream.Current);
            }
        }

        public Node VarDef() {

            var varToken = Expect(TokenCategory.VAR);

            var varList = VarList();
            
            Expect(TokenCategory.SEMICOLON);

            var result = new VarDef(){ varList };
            result.AnchorToken = varToken;

            return result;
        }

        public Node VarList() {
            var result = new VarList();

            result.Add(new Identifier() {
                AnchorToken = Expect(TokenCategory.IDENTIFIER)
            });

            while (CurrentToken==TokenCategory.LIST_ELEMENT) {
                Expect(TokenCategory.LIST_ELEMENT);
                result.Add(new Identifier() {
                    AnchorToken = Expect(TokenCategory.IDENTIFIER)
                });
            }

            return result;
        }

        public Node FunDef() {
            var funToken = Expect(TokenCategory.IDENTIFIER);
            Expect(TokenCategory.PARENTHESIS_OPEN);
            var parList = ParamList();
            Expect(TokenCategory.PARENTHESIS_CLOSE);
            Expect(TokenCategory.BRACKET_OPEN);
            var varDList = VarDefList();
            var statementList = StmtList();
            Expect(TokenCategory.BRACKET_CLOSE);

            var result = new FunDef() { parList, varDList, statementList };
            
            result.AnchorToken = funToken;

            return result;
        }

        public Node ParamList() {
            var result = new ParamList();

            if (CurrentToken==TokenCategory.IDENTIFIER){
                result.Add(new Identifier() {
                    AnchorToken = Expect(TokenCategory.IDENTIFIER)
                });

                while (CurrentToken==TokenCategory.LIST_ELEMENT) {
                    Expect(TokenCategory.LIST_ELEMENT);
                    result.Add(new Identifier() {
                        AnchorToken = Expect(TokenCategory.IDENTIFIER)
                    });
                }
            }

            return result;
        }

        public Node VarDefList() {
            var result = new VarDefList();
            while(CurrentToken==TokenCategory.VAR){
                result.Add(VarDef());
            }
            return result;
        }

        public Node StmtList() {
            var result = new StatementList();
            while(firstOfStatement.Contains(CurrentToken)){
                result.Add(Statement());
            }
            return result;
        }

        public Node Statement() {

            switch (CurrentToken) {

            case TokenCategory.IDENTIFIER:
                return StmtAsFun();

            case TokenCategory.INC:
                return StmtInc();

            case TokenCategory.DEC:
                return StmtDec();

            case TokenCategory.IF:
                return StmtIf();

            case TokenCategory.WHILE:
                return StmtWhile();

            case TokenCategory.DO:
                return StmtDo();

            case TokenCategory.BREAK:
                return StmtBreak();

            case TokenCategory.RETURN:
                return StmtReturn();

            case TokenCategory.SEMICOLON:
                Expect(TokenCategory.SEMICOLON);
                return new Empty();

            default:
                throw new SyntaxError(firstOfStatement,
                                      tokenStream.Current);
            }
        }

        public Node StmtAsFun(){
            var idToken = Expect(TokenCategory.IDENTIFIER);
            switch (CurrentToken) {

            case TokenCategory.ASSIGN:
                Expect(TokenCategory.ASSIGN);
                var expr = Expression();
                Expect(TokenCategory.SEMICOLON);
                var result = new Assignment(){expr};
                result.AnchorToken=idToken;
                return result;

            case TokenCategory.PARENTHESIS_OPEN:
                Expect(TokenCategory.PARENTHESIS_OPEN);
                var exprList = ExprList();
                Expect(TokenCategory.PARENTHESIS_CLOSE);
                Expect(TokenCategory.SEMICOLON);
                var result2 = new FuncCall(){ exprList };
                result2.AnchorToken = idToken;
                return result2;

            default:
                throw new SyntaxError(secondOfStatement,
                                      tokenStream.Current);
            }

        }

        public Node FunCall() {
            var idToken = Expect(TokenCategory.IDENTIFIER);
            Expect(TokenCategory.PARENTHESIS_OPEN);
            var exprList = ExprList();
            Expect(TokenCategory.PARENTHESIS_CLOSE);
            Expect(TokenCategory.SEMICOLON);
            var result = new FuncCall(){ exprList };
            result.AnchorToken = idToken;
            return result;
    
        }

        public Node StmtInc() {
            var result = new Inc(){
                    AnchorToken = Expect(TokenCategory.INC)
                };
            result.Add(new Identifier(){AnchorToken = Expect(TokenCategory.IDENTIFIER)});

            Expect(TokenCategory.SEMICOLON);
            return result;
        }

        public Node StmtDec() {
            var result = new Dec(){
                    AnchorToken = Expect(TokenCategory.DEC)
                };
            result.Add(new Identifier(){AnchorToken= Expect(TokenCategory.IDENTIFIER)});
            Expect(TokenCategory.SEMICOLON);
            return result;
        }

        public Node ExprList() {
            var result = new ExpressionList();
            if(firstOfExpression.Contains(CurrentToken)){
                result.Add(Expression());

                while (CurrentToken==TokenCategory.LIST_ELEMENT) {
                    Expect(TokenCategory.LIST_ELEMENT);
                    result.Add(Expression());
                }
            }
            return result;
        }

        public Node StmtIf() {
            var ifToken =  Expect(TokenCategory.IF);
            Expect(TokenCategory.PARENTHESIS_OPEN);
            var expr = Expression();
            Expect(TokenCategory.PARENTHESIS_CLOSE);
            Expect(TokenCategory.BRACKET_OPEN);
            var stmtList = StmtList();
            Expect(TokenCategory.BRACKET_CLOSE);
            var elif = ElseIfList();
            var els = Else();

            var result = new If() { expr, stmtList,elif,els };
            result.AnchorToken = ifToken;
            return result;
        }

        public Node ElseIfList() {
            var result = new ElseIfList();
            while(CurrentToken==TokenCategory.ELIF){
                var elifNode = new ElseIf();
                var elif= Expect(TokenCategory.ELIF);
                Expect(TokenCategory.PARENTHESIS_OPEN);
                var expr = Expression();
                elifNode.Add(expr);
                Expect(TokenCategory.PARENTHESIS_CLOSE);
                Expect(TokenCategory.BRACKET_OPEN);
                var stmtList = StmtList();
                elifNode.Add(stmtList);
                Expect(TokenCategory.BRACKET_CLOSE);

                elifNode.AnchorToken=elif;
                result.Add(elifNode);
            }
            return result;
        }

        public Node Else() {
            var result = new Else();
            if(CurrentToken==TokenCategory.ELSE){
                var elseVar = Expect(TokenCategory.ELSE);
                Expect(TokenCategory.BRACKET_OPEN);
                var stmtlist = StmtList();
                Expect(TokenCategory.BRACKET_CLOSE);
                result.Add(stmtlist);
                result.AnchorToken= elseVar;
            }
            return result;
        }

        public Node StmtWhile() {
            var whileToken = Expect(TokenCategory.WHILE);

            Expect(TokenCategory.PARENTHESIS_OPEN);

            var expr = Expression();
            Expect(TokenCategory.PARENTHESIS_CLOSE);
            Expect(TokenCategory.BRACKET_OPEN);
            var statementList = StmtList();
            Expect(TokenCategory.BRACKET_CLOSE);
            var result = new While(){expr, statementList};
            result.AnchorToken = whileToken;
            return result;
        }

        public Node StmtDo() {
            var doToken = Expect(TokenCategory.DO);

            Expect(TokenCategory.BRACKET_OPEN);
            var statementList = StmtList();
            Expect(TokenCategory.BRACKET_CLOSE);
            Expect(TokenCategory.WHILE);
            Expect(TokenCategory.PARENTHESIS_OPEN);
            var expr = Expression();
            Expect(TokenCategory.PARENTHESIS_CLOSE);
            Expect(TokenCategory.SEMICOLON);
            var result = new DoWhile(){statementList, expr};
            result.AnchorToken = doToken;
            return result;
        }

        public Node StmtBreak() {
            var result = new Break(){
                AnchorToken=Expect(TokenCategory.BREAK)
            };
            Expect(TokenCategory.SEMICOLON);
            return result;
        }

        public Node StmtReturn() {
            var returnToken = Expect(TokenCategory.RETURN);
            
            var expr = Expression();
            Expect(TokenCategory.SEMICOLON);
            var result = new Return(){expr};
            result.AnchorToken = returnToken;
            return result;
        }

        public Node Expression() {
            return ExpressionOr();
        }

        public Node ExpressionOr() {
            var exprAnd1 = ExpressionAnd();

            while(CurrentToken==TokenCategory.OR){
                var exprAnd2 = new Or(){
                    AnchorToken = Expect(TokenCategory.OR)
                };
                exprAnd2.Add(exprAnd1);
                exprAnd2.Add(ExpressionAnd());
                exprAnd1 = exprAnd2;
            }

            return exprAnd1;
        }

        public Node ExpressionAnd() {
            var expr1 = ExpressionComp();

            while(CurrentToken==TokenCategory.AND){
                var expr2 = new And(){
                    AnchorToken = Expect(TokenCategory.AND)
                };
                expr2.Add(expr1);
                expr2.Add(ExpressionComp());
                expr1 = expr2;
            }

            return expr1;
        }

        public Node ExpressionComp() {
            var expr1 = ExpressionRel();

            while(firstOfOperator.Contains(CurrentToken)){
                var expr2 = OpComp();
                expr2.Add(expr1);
                expr2.Add(ExpressionRel());
                expr1 = expr2;
            }

            return expr1;
        }

        public Node OpComp() {
            switch (CurrentToken) {
                
            case TokenCategory.EQUAL_TO:
                return new EqualTo(){
                    AnchorToken = Expect(TokenCategory.EQUAL_TO)
                };

            case TokenCategory.OP_COMP:
                return new OpComp(){
                     AnchorToken = Expect(TokenCategory.OP_COMP)
                };

            default:
                throw new SyntaxError(firstOfOperator,
                                      tokenStream.Current);
            }
        }

        public Node ExpressionRel() {
            var expr1 = ExpressionAdd();

            while(firstOfOperatorRel.Contains(CurrentToken)){
                var expr2 = OpRel();
                expr2.Add(expr1);
                expr2.Add(ExpressionAdd());
                expr1 = expr2;
            }

            return expr1;
        }

        public Node OpRel() {
            switch (CurrentToken) {

            case TokenCategory.MORE:
                return new More(){
                    AnchorToken = Expect(TokenCategory.MORE)
                };

            case TokenCategory.MORE_EQUAL:
                return new MoreEqual(){
                    AnchorToken = Expect(TokenCategory.MORE_EQUAL)
                };
            
            case TokenCategory.LESS:
                return new Less(){
                    AnchorToken = Expect(TokenCategory.LESS)
                };

            case TokenCategory.LESS_EQUAL:
                
                return new LessEqual(){
                    AnchorToken = Expect(TokenCategory.LESS_EQUAL)
                };

            default:
                throw new SyntaxError(firstOfOperatorRel,
                                      tokenStream.Current);
            }
        }

        public Node ExpressionAdd() {
            var expr1 = ExpressionMul();

            while(firstOfOperatorAdd.Contains(CurrentToken)){
                var expr2 = OpAdd();
                expr2.Add(expr1);
                expr2.Add(ExpressionMul());
                expr1 = expr2;
            }

            return expr1;
        }

        public Node OpAdd() {
            switch (CurrentToken) {

            case TokenCategory.ADD:
                return new Add(){
                    AnchorToken = Expect(TokenCategory.ADD)
                };

            case TokenCategory.SUBTR:
                return new Subtr(){
                    AnchorToken = Expect(TokenCategory.SUBTR)
                };

            default:
                throw new SyntaxError(firstOfOperatorAdd,
                                      tokenStream.Current);
            }
        }

        public Node ExpressionMul() {
            var expr1 = ExpressionUnary();

            while(firstOfOperatorMul.Contains(CurrentToken)){
                var expr2 = OpMul();
                expr2.Add(expr1);
                expr2.Add(ExpressionUnary());
                expr1 = expr2;
            }

            return expr1;
        }

        public Node OpMul() {
            switch (CurrentToken) {

            case TokenCategory.MUL:
                return new Mul(){
                    AnchorToken = Expect(TokenCategory.MUL)
                };

            case TokenCategory.DIV:
                return new Div(){
                    AnchorToken = Expect(TokenCategory.DIV)
                };
            
            case TokenCategory.MOD:
                return new Mod(){
                    AnchorToken = Expect(TokenCategory.MOD)
                };

            default:
                throw new SyntaxError(firstOfOperatorMul,
                                      tokenStream.Current);
            }
        }

        public Node ExpressionUnary() {
            if(firstOfOperatorUnary.Contains(CurrentToken)){
                var result = new ExpressionUnary();
                var opUL = new OpUnaryList();

                while(firstOfOperatorUnary.Contains(CurrentToken)){
                    opUL.Add(OpUnary());
                }
                result.Add(opUL);

                var expr = ExpressionPrimary();
                result.Add(expr);
                return result;
            }
            return ExpressionPrimary();
        }

        public Node OpUnary() {
            switch (CurrentToken) {

            case TokenCategory.ADD:
                return new Add(){
                    AnchorToken = Expect(TokenCategory.ADD)
                };

            case TokenCategory.SUBTR:
                return new Subtr(){
                    AnchorToken = Expect(TokenCategory.SUBTR)
                };
            
            case TokenCategory.NOT:
                return new Not(){
                    AnchorToken = Expect(TokenCategory.NOT)
                };
            default:
                throw new SyntaxError(firstOfOperatorUnary,
                                      tokenStream.Current);
            }
        }

        public Node ExpressionPrimary() {
            switch (CurrentToken) {

            case TokenCategory.IDENTIFIER:
                var idToken = Expect(TokenCategory.IDENTIFIER);

                if(CurrentToken==TokenCategory.PARENTHESIS_OPEN){
                    Expect(TokenCategory.PARENTHESIS_OPEN);
                    var exprList = ExprList();
                    Expect(TokenCategory.PARENTHESIS_CLOSE);
                    var result = new FuncCall(){ exprList };
                    result.AnchorToken = idToken;
                    return result;
                }
                else{
                    return new Identifier(){
                        AnchorToken = idToken
                    };
                }
            
            case TokenCategory.INT_LITERAL:
                return new IntLiteral(){
                    AnchorToken = Expect(TokenCategory.INT_LITERAL)
                };

            case TokenCategory.STRING:
                return new String(){
                    AnchorToken = Expect(TokenCategory.STRING)
                };
            
            case TokenCategory.CHAR:
                return new Char(){
                    AnchorToken = Expect(TokenCategory.CHAR)
                };
            
            case TokenCategory.UNICODE_CHAR:
                return new UnicodeChar(){
                    AnchorToken = Expect(TokenCategory.UNICODE_CHAR)
                };
            
            case TokenCategory.TRUE:
                return new True(){
                    AnchorToken = Expect(TokenCategory.TRUE)
                };

            case TokenCategory.FALSE:
                return new False(){
                    AnchorToken = Expect(TokenCategory.FALSE)
                };

            case TokenCategory.SQUARE_OPEN:
                return Array();
            
            case TokenCategory.PARENTHESIS_OPEN:
                Expect(TokenCategory.PARENTHESIS_OPEN);
                var result2 = Expression();
                Expect(TokenCategory.PARENTHESIS_CLOSE);
                return result2;

            default:
                throw new SyntaxError(firstOfExpression,
                                      tokenStream.Current);
            }
        }

        public Node Array() {
            Expect(TokenCategory.SQUARE_OPEN);
            var exprlist = ExprList();
            Expect(TokenCategory.SQUARE_CLOSE);
            var result = new Array(){exprlist};
            return result;
        }
    }
}
