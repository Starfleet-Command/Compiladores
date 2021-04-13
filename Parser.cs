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

        public void Program() {

            while (firstOfDefinition.Contains(CurrentToken)) {
                Definition();
            }

            Expect(TokenCategory.EOF);
        }

        public void Definition() {
            switch (CurrentToken) {

            case TokenCategory.VAR:
                VarDef();
                break;

            case TokenCategory.IDENTIFIER:
                FunDef();
                break;

            default:
                throw new SyntaxError(firstOfDefinition,
                                      tokenStream.Current);
            }
        }

        public void VarDef() {
            Expect(TokenCategory.VAR);
            VarList();
            Expect(TokenCategory.SEMICOLON);
        }

        public void VarList() {
            IdList();
        }

        public void IdList() {
            Expect(TokenCategory.IDENTIFIER);

            while (CurrentToken==TokenCategory.LIST_ELEMENT) {
                Expect(TokenCategory.LIST_ELEMENT);
                Expect(TokenCategory.IDENTIFIER);
            }

        }

        public void FunDef() {
            Expect(TokenCategory.IDENTIFIER);
            Expect(TokenCategory.PARENTHESIS_OPEN);
            ParamList();
            Expect(TokenCategory.PARENTHESIS_CLOSE);
            Expect(TokenCategory.BRACKET_OPEN);
            VarDefList();
            StmtList();
            Expect(TokenCategory.BRACKET_CLOSE);
        }

        public void ParamList() {
            if (CurrentToken==TokenCategory.IDENTIFIER){
                IdList();
            }
        }

        public void VarDefList() {
            while(CurrentToken==TokenCategory.VAR){
                VarDef();
            }
        }

        public void StmtList() {
            while(firstOfStatement.Contains(CurrentToken)){
                Statement();
            }
        }

        public void Statement() {

            switch (CurrentToken) {

            case TokenCategory.IDENTIFIER:
                Expect(TokenCategory.IDENTIFIER);
                switch (CurrentToken) {

                    case TokenCategory.ASSIGN:
                        StmtAssignSecond();
                        break;

                    case TokenCategory.PARENTHESIS_OPEN:
                        StmtFunCallSecond();
                        break;

                    default:
                        throw new SyntaxError(secondOfStatement,
                                            tokenStream.Current);
                    }
                break;

            case TokenCategory.INC:
                StmtInc();
                break;

            case TokenCategory.DEC:
                StmtDec();
                break;

            case TokenCategory.IF:
                StmtIf();
                break;

            case TokenCategory.WHILE:
                StmtWhile();
                break;

            case TokenCategory.DO:
                StmtDo();
                break;

            case TokenCategory.BREAK:
                StmtBreak();
                break;

            case TokenCategory.RETURN:
                StmtReturn();
                break;

            case TokenCategory.SEMICOLON:
                StmtEmpty();
                break;

            default:
                throw new SyntaxError(firstOfStatement,
                                      tokenStream.Current);
            }
        }

        public void StmtAssignSecond() {
            Expect(TokenCategory.ASSIGN);
            Expression();
            Expect(TokenCategory.SEMICOLON);
        }

        public void StmtInc() {
            Expect(TokenCategory.INC);
            Expect(TokenCategory.IDENTIFIER);
            Expect(TokenCategory.SEMICOLON);
        }

        public void StmtDec() {
            Expect(TokenCategory.DEC);
            Expect(TokenCategory.IDENTIFIER);
            Expect(TokenCategory.SEMICOLON);
        }

        public void StmtFunCallSecond() {
            FunCallSecond();
            Expect(TokenCategory.SEMICOLON);
        }

        public void FunCall() {
            Expect(TokenCategory.IDENTIFIER);
            Expect(TokenCategory.PARENTHESIS_OPEN);
            ExprList();
            Expect(TokenCategory.PARENTHESIS_CLOSE);
    
        }

        public void FunCallSecond() {
            Expect(TokenCategory.PARENTHESIS_OPEN);
            ExprList();
            Expect(TokenCategory.PARENTHESIS_CLOSE);
        }

        public void ExprList() {
            if(CurrentToken!=TokenCategory.PARENTHESIS_CLOSE){
                Expression();

                while (CurrentToken==TokenCategory.LIST_ELEMENT) {
                    Expect(TokenCategory.LIST_ELEMENT);
                    Expression();
                }
            }
        }

        public void StmtIf() {
            Expect(TokenCategory.IF);

            Expect(TokenCategory.PARENTHESIS_OPEN);
            Expression();
            Expect(TokenCategory.PARENTHESIS_CLOSE);
            Expect(TokenCategory.BRACKET_OPEN);
            StmtList();
            Expect(TokenCategory.BRACKET_CLOSE);
            ElseIfList();
            Else();
        }

        public void ElseIfList() {
            while(CurrentToken==TokenCategory.ELIF){
                Expect(TokenCategory.ELIF);
                Expect(TokenCategory.PARENTHESIS_OPEN);
                Expression();
                Expect(TokenCategory.PARENTHESIS_CLOSE);
                Expect(TokenCategory.BRACKET_OPEN);
                StmtList();
                Expect(TokenCategory.BRACKET_CLOSE);
            }
        }

        public void Else() {
            if(CurrentToken==TokenCategory.ELSE){
                Expect(TokenCategory.ELSE);
                Expect(TokenCategory.BRACKET_OPEN);
                StmtList();
                Expect(TokenCategory.BRACKET_CLOSE);
            }
        }

        public void StmtWhile() {
            Expect(TokenCategory.WHILE);

            Expect(TokenCategory.PARENTHESIS_OPEN);
            Expression();
            Expect(TokenCategory.PARENTHESIS_CLOSE);
            Expect(TokenCategory.BRACKET_OPEN);
            StmtList();
            Expect(TokenCategory.BRACKET_CLOSE);
        }

        public void StmtDo() {
            Expect(TokenCategory.DO);

            Expect(TokenCategory.BRACKET_OPEN);
            StmtList();
            Expect(TokenCategory.BRACKET_CLOSE);
            Expect(TokenCategory.WHILE);
            Expect(TokenCategory.PARENTHESIS_OPEN);
            Expression();
            Expect(TokenCategory.PARENTHESIS_CLOSE);
            Expect(TokenCategory.SEMICOLON);
        }

        public void StmtBreak() {
            Expect(TokenCategory.BREAK);
            Expect(TokenCategory.SEMICOLON);
        }

        public void StmtReturn() {
            Expect(TokenCategory.RETURN);
            Expression();
            Expect(TokenCategory.SEMICOLON);
        }

        public void StmtEmpty() {
            Expect(TokenCategory.SEMICOLON);
        }

        public void Expression() {
            ExpressionOr();
        }

        public void ExpressionOr() {
            ExpressionAnd();

            while(CurrentToken==TokenCategory.OR){
                Expect(TokenCategory.OR);
                ExpressionAnd();
            }
        }

        public void ExpressionAnd() {
            ExpressionComp();

            while(CurrentToken==TokenCategory.AND){
                Expect(TokenCategory.AND);
                ExpressionComp();
            }
        }

        public void ExpressionComp() {
            ExpressionRel();

            while(firstOfOperator.Contains(CurrentToken)){
                OpComp();
                ExpressionRel();
            }
        }

        public void OpComp() {
            switch (CurrentToken) {

            case TokenCategory.EQUAL_TO:
                Expect(TokenCategory.EQUAL_TO);
                break;

            case TokenCategory.OP_COMP:
                Expect(TokenCategory.OP_COMP);
                break;

            default:
                throw new SyntaxError(firstOfOperator,
                                      tokenStream.Current);
            }
        }

        public void ExpressionRel() {
            ExpressionAdd();

            while(firstOfOperatorRel.Contains(CurrentToken)){
                OpRel();
                ExpressionAdd();
            }
        }

        public void OpRel() {
            switch (CurrentToken) {

            case TokenCategory.MORE:
                Expect(TokenCategory.MORE);
                break;

            case TokenCategory.MORE_EQUAL:
                Expect(TokenCategory.MORE_EQUAL);
                break;
            
            case TokenCategory.LESS:
                Expect(TokenCategory.LESS);
                break;

            case TokenCategory.LESS_EQUAL:
                Expect(TokenCategory.LESS_EQUAL);
                break;

            default:
                throw new SyntaxError(firstOfOperatorRel,
                                      tokenStream.Current);
            }
        }

        public void ExpressionAdd() {
            ExpressionMul();

            while(firstOfOperatorAdd.Contains(CurrentToken)){
                OpAdd();
                ExpressionMul();
            }
        }

        public void OpAdd() {
            switch (CurrentToken) {

            case TokenCategory.ADD:
                Expect(TokenCategory.ADD);
                break;

            case TokenCategory.SUBTR:
                Expect(TokenCategory.SUBTR);
                break;

            default:
                throw new SyntaxError(firstOfOperatorAdd,
                                      tokenStream.Current);
            }
        }

        public void ExpressionMul() {
            ExpressionUnary();

            while(firstOfOperatorMul.Contains(CurrentToken)){
                OpMul();
                ExpressionUnary();
            }
        }

        public void OpMul() {
            switch (CurrentToken) {

            case TokenCategory.MUL:
                Expect(TokenCategory.MUL);
                break;

            case TokenCategory.DIV:
                Expect(TokenCategory.DIV);
                break;
            
            case TokenCategory.MOD:
                Expect(TokenCategory.MOD);
                break;

            default:
                throw new SyntaxError(firstOfOperatorMul,
                                      tokenStream.Current);
            }
        }

        public void ExpressionUnary() {
            while(firstOfOperatorUnary.Contains(CurrentToken)){
                OpUnary();
            }
            ExpressionPrimary();
        }

        public void OpUnary() {
            switch (CurrentToken) {

            case TokenCategory.ADD:
                Expect(TokenCategory.ADD);
                break;

            case TokenCategory.SUBTR:
                Expect(TokenCategory.SUBTR);
                break;
            
            case TokenCategory.NOT:
                Expect(TokenCategory.NOT);
                break;

            default:
                throw new SyntaxError(firstOfOperatorUnary,
                                      tokenStream.Current);
            }
        }

        public void ExpressionPrimary() {
            switch (CurrentToken) {

            case TokenCategory.IDENTIFIER:
                Expect(TokenCategory.IDENTIFIER);

                if(CurrentToken==TokenCategory.PARENTHESIS_OPEN){
                    FunCallSecond();
                }
                break;
            
            case TokenCategory.INT_LITERAL:
                Expect(TokenCategory.INT_LITERAL);
                break;

            case TokenCategory.STRING:
                Expect(TokenCategory.STRING);
                break;
            
            case TokenCategory.CHAR:
                Expect(TokenCategory.CHAR);
                break;
            
            case TokenCategory.UNICODE_CHAR:
                Expect(TokenCategory.UNICODE_CHAR);
                break;
            
            case TokenCategory.TRUE:
                Expect(TokenCategory.TRUE);
                break;

            case TokenCategory.FALSE:
                Expect(TokenCategory.FALSE);
                break;

            case TokenCategory.SQUARE_OPEN:
                Array();
                break;
            
            case TokenCategory.PARENTHESIS_OPEN:
                Expect(TokenCategory.PARENTHESIS_OPEN);
                Expression();
                Expect(TokenCategory.PARENTHESIS_CLOSE);
                break;

            default:
                throw new SyntaxError(firstOfOperatorUnary,
                                      tokenStream.Current);
            }
        }

        public void Array() {
            Expect(TokenCategory.SQUARE_OPEN);
            ExprList();
            Expect(TokenCategory.SQUARE_CLOSE);
        }
    }
}
