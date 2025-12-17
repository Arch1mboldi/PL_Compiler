#ifndef COMMON_H
#define COMMON_H

#include <string>
#include <vector>

using namespace std;

enum TokenType {
    T_NULL, T_IDENT, T_NUMBER, T_STRING, T_PROGRAM,
    // Keywords
    T_CONST, T_VAR, T_PROCEDURE, T_FUNCTION, T_BEGIN, T_END, T_IF, T_THEN, 
    T_ELSE, T_WHILE, T_DO, T_CALL, T_READ, T_WRITE, T_EXIT, T_ODD, 
    T_TYPE, T_ARRAY, T_OF, T_OR, T_AND, T_NOT, T_DIV, T_MOD,
    T_TRUE, T_FALSE, T_INTEGER, T_REAL, T_BOOLEAN,
    // Operators & Delimiters
    T_PLUS, T_MINUS, T_MUL, T_SLASH, T_EQ, T_NEQ, T_LT, T_LE, T_GT, T_GE,
    T_LPAREN, T_RPAREN, T_LBRACKET, T_RBRACKET, T_COMMA, T_SEMICOLON, 
    T_PERIOD, T_ASSIGN, T_COLON, T_DOTDOT, T_EOF
};

struct Token {
    int id;
    int line;
    TokenType type;
    string value; 
    int intVal;   
    double realVal; 
};

struct Error {
    int code;
    int line;
    string msg;
};

struct Quadruple {
    int id;
    string op;
    string arg1;
    string arg2;
    string result;
};

#endif
