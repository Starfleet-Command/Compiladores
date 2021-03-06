/*
  Drac compiler - Token categories for the scanner.
*/

namespace Buttercup {

    enum TokenCategory {
        ADD, //
        AND, //
        ASSIGN, //
        BACKSLASH, //
        BRACKET_OPEN, //
        BRACKET_CLOSE, //
        BREAK, //
        CHAR, //
        CARRIAGE_RETURN, //
        DEC, //
        DIV, //
        DQUOTE, // PREGUNTAR
        DO, //
        ELIF, //
        ELSE, //
        EOF, //PREGUNTAR
        EQUAL_TO, //
        FALSE, //
        IDENTIFIER, //
        IF, //
        ILLEGAL_CHAR, //
        INC, //
        INT_LITERAL, //
        LIST_ELEMENT, //
        LESS, //
        LESS_EQUAL, //
        MUL, //
        MULTI_COMMENT, // 
        MOD, //
        MORE, //
        MORE_EQUAL, //
        NOT, //
        NEWLINE, //
        PARENTHESIS_OPEN, //
        PARENTHESIS_CLOSE, //
        OP_COMP, //
        OR, //
        RETURN, //
        SEMICOLON, //
        SINGLE_COMMENT, //
        SUBTR, //
        STRING, //
        SQUOTE, //
        SQUARE_OPEN, //
        SQUARE_CLOSE, //
        TAB, //
        THEN, //
        TRUE, //
        UNICODE_CHAR, //
        VAR, //
        WHILE, //
        WHITESPACE, //
    }
}
