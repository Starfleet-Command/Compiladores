/*
  Drac compiler - This class performs the lexical analysis,
  (a.k.a. scanning).

  Rubén Sánchez             A01021759
  Juan Francisco Gortarez   A01021926
  Martín Alegría            A01022216
*/

using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace Drac {

    class Scanner {

        readonly string input;

        static readonly Regex regex = new Regex(
            @"
                (?<ListElement>       ,       )
              | (?<BracketOpen>       \{       )
              | (?<BracketClose>       \}      )
              | (?<SqBracketOpen>       \[       )
              | (?<SqBracketClose>       \]      )
              | (?<CarriageReturn>       \\r      )
              | (?<MultiComment>        \(\*[\s\S]*?\*\)     )
              | (?<Elif>       elif\b       )
              | (?<Else>       else\b       )
              | (?<Newline>    \n        )
              | (?<WhiteSpace> \s        )     # Must go after Newline.
              | (?<And>        and\b       )
              | (?<OpComp>       <>       )
              | (?<LessEqual>       <=       )
              | (?<Less>       [<]       )
              | (?<MoreEqual>       >=       )
              | (?<More>       [>]       )
              | (?<Add>       [+]       )
              | (?<Mul>        [*]       )
              | (?<SingleComment>        \-\-.*       )
              | (?<Mod>        [%]       )
              | (?<Subtract>        \-       )
              | (?<ParOpen>    [(]       )
              | (?<ParClose>   [)]       )
              | (?<EqualTo>    ==       )
              | (?<Assign>     =       )
              | (?<True>       true\b      )
              | (?<False>      false\b    )
              | (?<IntLiteral> \-?[0-9]\d*      )
              | (?<Decrease>       dec\b      )
              | (?<Increase>       inc\b      )
              | (?<Div>       [/]      )
              | (?<Or>         or\b       )
              | (?<Not>        not\b       )
              | (?<Var>        var\b      )
              | (?<Return>     return\b       )
              | (?<Do>         do\b       )
              | (?<Semicolon>  [;]       )
              | (?<Break>      break\b       )
              | (?<If>         if\b        )
              | (?<While>      while\b        )
              | (?<UnicodeChar> '\\u[a-fA-F0-9]{6}'       )
              | (?<String>      ""[^""\\\n]*(?:\\.[^""\\\n]*)*""      )
              | (?<Char>      '\\[tnr\\'""]'|'[^']'       )
              | (?<Identifier> [a-zA-Z0-9_]+ )     # Must go after all keywords
              | (?<IllegalChar>      .         )     # Must be last: match any other character.
            ",
            RegexOptions.IgnorePatternWhitespace
                | RegexOptions.Compiled
                | RegexOptions.Multiline
                
            );

        static readonly IDictionary<string, TokenCategory> tokenMap =
            new Dictionary<string, TokenCategory>() {
                {"And", TokenCategory.AND},
                {"Add", TokenCategory.ADD},
                {"Assign", TokenCategory.ASSIGN},
                {"BracketOpen", TokenCategory.BRACKET_OPEN},
                {"BracketClose", TokenCategory.BRACKET_CLOSE},
                {"Break", TokenCategory.BREAK},
                {"CarriageReturn", TokenCategory.CARRIAGE_RETURN},
                {"Char", TokenCategory.CHAR},
                {"Decrease", TokenCategory.DEC},
                {"Div", TokenCategory.DIV},
                {"Do", TokenCategory.DO},
                {"Elif", TokenCategory.ELIF},
                {"Else", TokenCategory.ELSE},
                {"EqualTo", TokenCategory.EQUAL_TO},
                {"False", TokenCategory.FALSE},
                {"Identifier", TokenCategory.IDENTIFIER},
                {"If", TokenCategory.IF},
                {"IllegalChar", TokenCategory.ILLEGAL_CHAR},
                {"IntLiteral", TokenCategory.INT_LITERAL},
                {"Increase", TokenCategory.INC},
                {"ListElement", TokenCategory.LIST_ELEMENT},
                {"Less", TokenCategory.LESS},
                {"LessEqual", TokenCategory.LESS_EQUAL},
                {"Mul", TokenCategory.MUL},
                {"MultiComment", TokenCategory.MULTI_COMMENT},
                {"Mod", TokenCategory.MOD},
                {"More", TokenCategory.MORE},
                {"MoreEqual", TokenCategory.MORE_EQUAL},
                {"Newline", TokenCategory.NEWLINE},
                {"Not", TokenCategory.NOT},
                {"ParOpen", TokenCategory.PARENTHESIS_OPEN},
                {"ParClose", TokenCategory.PARENTHESIS_CLOSE},
                {"OpComp", TokenCategory.OP_COMP},
                {"Or", TokenCategory.OR},
                {"Return", TokenCategory.RETURN},
                {"Semicolon", TokenCategory.SEMICOLON},
                {"SingleComment", TokenCategory.SINGLE_COMMENT},
                {"Subtract", TokenCategory.SUBTR},
                {"String", TokenCategory.STRING},
                {"SqBracketOpen", TokenCategory.SQUARE_OPEN},
                {"SqBracketClose", TokenCategory.SQUARE_CLOSE},
                {"True", TokenCategory.TRUE},
                {"UnicodeChar", TokenCategory.UNICODE_CHAR},
                {"Var", TokenCategory.VAR},
                {"While", TokenCategory.WHILE},
                {"WhiteSpace", TokenCategory.WHITESPACE}
            };

        public Scanner(string input) {
            this.input = input;
        }

        public IEnumerable<Token> Scan() {

            var result = new LinkedList<Token>();
            var row = 1;
            var columnStart = 0;

            foreach (Match m in regex.Matches(input)) {

                if (m.Groups["Newline"].Success) {

                    row++;
                    columnStart = m.Index + m.Length;

                } else if (m.Groups["WhiteSpace"].Success || m.Groups["SingleComment"].Success) 
                {

                }

                else if(m.Groups["MultiComment"].Success)
                {
                    string texto= m.ToString();
                    int numLines = texto.Split('\n').Length;
                    row = row+(numLines-1);

                }

                    // Skip white space and comments.

                 else if (m.Groups["Other"].Success) {

                    // Found an illegal character.
                    result.AddLast(
                        new Token(m.Value,
                            TokenCategory.ILLEGAL_CHAR,
                            row,
                            m.Index - columnStart + 1));

                } else {

                    // Must be any of the other tokens.
                    result.AddLast(FindToken(m, row, columnStart));
                }
            }

            result.AddLast(
                new Token(null,
                    TokenCategory.EOF,
                    row,
                    input.Length - columnStart + 1));

            return result;
        }

        Token FindToken(Match m, int row, int columnStart) {
            foreach (var name in tokenMap.Keys) {
                if (m.Groups[name].Success) {
                    return new Token(m.Value,
                        tokenMap[name],
                        row,
                        m.Index - columnStart + 1);
                }
            }
            throw new InvalidOperationException(
                "regex and tokenMap are inconsistent: " + m.Value);
        }
    }
}
