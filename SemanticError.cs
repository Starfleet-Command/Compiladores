/*
  Drac Semantic Error class

  Rubén Sánchez             A01021759
  Juan Francisco Gortarez   A01021926
  Martín Alegría            A01022216
*/

using System;

namespace Drac {

    class SemanticError: Exception {

        public SemanticError(string message, Token token):
            base($"Semantic Error: {message} \n"
                 + $"at row {token.Row}, column {token.Column}.") {
        }
    }
}
