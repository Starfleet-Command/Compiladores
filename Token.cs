/*
  Drac compiler - Token class for the scanner.

  Rubén Sánchez             A01021759
  Juan Francisco Gortarez   A01021926
  Martín Alegría            A01022216
*/

namespace Drac {

    class Token {

        readonly string lexeme;

        readonly TokenCategory category;

        readonly int row;

        readonly int column;

        public string Lexeme {
            get { return lexeme; }
        }

        public TokenCategory Category {
            get { return category; }
        }

        public int Row {
            get { return row; }
        }

        public int Column {
            get { return column; }
        }

        public Token(string lexeme,
                     TokenCategory category,
                     int row,
                     int column) {
            this.lexeme = lexeme;
            this.category = category;
            this.row = row;
            this.column = column;
        }

        public override string ToString() {
            return $"{{{category}, \"{lexeme}\", @({row}, {column})}}";
        }
    }
}
