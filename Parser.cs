/*
  Buttercup compiler - This class performs the syntactic analysis,
  (a.k.a. parsing).
  Copyright (C) 2013-2021 Ariel Ortiz, ITESM CEM

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
 *  Program       ::= DefList
 *  DefList       ::= Def*
 *  Def             ::= Var-Def | Fun-Def
 *  Var-Def         ::= "var" Var-List ";"
 *  Var-List      ::= ‹id-list›
 *  ‹id-list›       ::= <id> ("," <id>)*
 *  Fun-Def         ::= <id> "(" ‹param-list› ")" "{" ‹var-def-list› ‹stmt-list› "}"
 *  ‹param-list›    ::= ‹id-list›*
 *  ‹var-def-list›  ::= <var-def>+
 *  ‹stmt-list›     ::= ‹stmt›+
 *  ‹stmt›          ::= ‹stmt-assign› | ‹stmt-incr› | ‹stmt-decr› | ‹stmt-fun-call› | ‹stmt-if› | ‹stmt-while› | ‹stmt-do-while› | ‹stmt-break› | ‹stmt-return› | ‹stmt-empty›
 *  ‹stmt-assign›   ::= <id> "=" ‹expr› ";"
 *  ‹stmt-incr›     ::= "inc" <id> ";"
 *  ‹stmt-decr›     ::= "dec" <id> ";"
 *  ‹stmt-fun-call› ::= ‹fun-call› ";"
 *  ‹fun-call›      ::= <id> "(" ‹expr-list› ")"
 *  ‹expr-list›     ::= <expr> ("," <expr>)*
 *  ‹stmt-if›       ::= "if" "(" ‹expr› ")" "{" ‹stmt-list› "}" ‹else-if-list› ‹else›
 *  ‹else-if-list›  ::= ("elif" "(" ‹expr› ")" "{" ‹stmt-list› "}")*
 *  ‹else›          ::= ("else" "{" ‹stmt-list› "}")?
 *  ‹stmt-while›    ::= "while" "(" ‹expr› ")" "{" ‹stmt-list› "}"
 *  ‹stmt-do-while› ::= "do" "{" ‹stmt-list› "}" "while" "(" ‹expr› ")" ";"
 *  ‹stmt-break›    ::= "break" ";"
 *  ‹stmt-return›   ::=	"return" ‹expr› ";"
 *  ‹stmt-empty›    ::= ";"
 *  ‹expr›          ::= ‹expr-or›
 *  ‹expr-or›       ::= ‹expr-and› ("or" ‹expr-and›)+
 *  ‹expr-and›      ::= ‹expr-comp› ("and" ‹expr-comp›)+
 *  ‹expr-comp›     ::= ‹expr-rel› (‹op-comp› ‹expr-rel›)+
 *  ‹op-comp›       ::= "==" | "<>"
 *  ‹expr-rel›      ::= ‹expr-add› (‹op-rel› ‹expr-add›)+
 *  ‹op-rel›        ::= "<" | ">" | "<=" | ">="
 *  ‹expr-add›      ::= ‹expr-mul› (‹op-add› ‹expr-mul›)+
 *  ‹op-add›        ::= "+" | "-"
 *  ‹expr-mul›      ::= ‹expr-unary› (‹op-mul› ‹expr-unary›)+
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

        static readonly ISet<TokenCategory> firstOfDeclaration =
            new HashSet<TokenCategory>() {
                TokenCategory.INT,
                TokenCategory.BOOL
            };

        static readonly ISet<TokenCategory> firstOfStatement =
            new HashSet<TokenCategory>() {
                TokenCategory.IDENTIFIER,
                TokenCategory.PRINT,
                TokenCategory.IF
            };

        static readonly ISet<TokenCategory> firstOfOperator =
            new HashSet<TokenCategory>() {
                TokenCategory.AND,
                TokenCategory.LESS,
                TokenCategory.PLUS,
                TokenCategory.MUL
            };

        static readonly ISet<TokenCategory> firstOfSimpleExpression =
            new HashSet<TokenCategory>() {
                TokenCategory.IDENTIFIER,
                TokenCategory.INT_LITERAL,
                TokenCategory.TRUE,
                TokenCategory.FALSE,
                TokenCategory.PARENTHESIS_OPEN,
                TokenCategory.NEG
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

            while (firstOfDeclaration.Contains(CurrentToken)) {
                Declaration();
            }

            while (firstOfStatement.Contains(CurrentToken)) {
                Statement();
            }

            Expect(TokenCategory.EOF);
        }

        public void Declaration() {
            Type();
            Expect(TokenCategory.IDENTIFIER);
        }

        public void Statement() {

            switch (CurrentToken) {

            case TokenCategory.IDENTIFIER:
                Assignment();
                break;

            case TokenCategory.PRINT:
                Print();
                break;

            case TokenCategory.IF:
                If();
                break;

            default:
                throw new SyntaxError(firstOfStatement,
                                      tokenStream.Current);
            }
        }

        public void Type() {
            switch (CurrentToken) {

            case TokenCategory.INT:
                Expect(TokenCategory.INT);
                break;

            case TokenCategory.BOOL:
                Expect(TokenCategory.BOOL);
                break;

            default:
                throw new SyntaxError(firstOfDeclaration,
                                      tokenStream.Current);
            }
        }

        public void Assignment() {
            Expect(TokenCategory.IDENTIFIER);
            Expect(TokenCategory.ASSIGN);
            Expression();
        }

        public void Print() {
            Expect(TokenCategory.PRINT);
            Expression();
        }

        public void If() {
            Expect(TokenCategory.IF);
            Expression();
            Expect(TokenCategory.THEN);
            while (firstOfStatement.Contains(CurrentToken)) {
                Statement();
            }
            Expect(TokenCategory.END);
        }

        public void Expression() {
            SimpleExpression();
            while (firstOfOperator.Contains(CurrentToken)) {
                Operator();
                SimpleExpression();
            }
        }

        public void SimpleExpression() {

            switch (CurrentToken) {

            case TokenCategory.IDENTIFIER:
                Expect(TokenCategory.IDENTIFIER);
                break;

            case TokenCategory.INT_LITERAL:
                Expect(TokenCategory.INT_LITERAL);
                break;

            case TokenCategory.TRUE:
                Expect(TokenCategory.TRUE);
                break;

            case TokenCategory.FALSE:
                Expect(TokenCategory.FALSE);
                break;

            case TokenCategory.PARENTHESIS_OPEN:
                Expect(TokenCategory.PARENTHESIS_OPEN);
                Expression();
                Expect(TokenCategory.PARENTHESIS_CLOSE);
                break;

            case TokenCategory.NEG:
                Expect(TokenCategory.NEG);
                SimpleExpression();
                break;

            default:
                throw new SyntaxError(firstOfSimpleExpression,
                                      tokenStream.Current);
            }
        }

        public void Operator() {

            switch (CurrentToken) {

            case TokenCategory.AND:
                Expect(TokenCategory.AND);
                break;

            case TokenCategory.LESS:
                Expect(TokenCategory.LESS);
                break;

            case  TokenCategory.PLUS:
                Expect(TokenCategory.PLUS);
                break;

            case TokenCategory.MUL:
                Expect(TokenCategory.MUL);
                break;

            default:
                throw new SyntaxError(firstOfOperator,
                                      tokenStream.Current);
            }
        }
    }
}
