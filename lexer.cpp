#include "lexer.h"
#include <cctype>
#include <iostream>

using namespace std;

Lexer::Lexer(string source) : src(source), pos(0), line(1), tokenCount(0) {
    initKeywords();
}

void Lexer::initKeywords() {
    keywords["program"] = T_PROGRAM;
    keywords["const"] = T_CONST; keywords["var"] = T_VAR;
    keywords["procedure"] = T_PROCEDURE; keywords["function"] = T_FUNCTION;
    keywords["begin"] = T_BEGIN; keywords["end"] = T_END;
    keywords["if"] = T_IF; keywords["then"] = T_THEN;
    keywords["else"] = T_ELSE; keywords["while"] = T_WHILE;
    keywords["do"] = T_DO; keywords["call"] = T_CALL;
    keywords["read"] = T_READ; keywords["write"] = T_WRITE;
    keywords["exit"] = T_EXIT; keywords["type"] = T_TYPE;
    keywords["array"] = T_ARRAY; keywords["of"] = T_OF;
    keywords["integer"] = T_INTEGER; keywords["real"] = T_REAL;
    keywords["boolean"] = T_BOOLEAN; keywords["true"] = T_TRUE;
    keywords["false"] = T_FALSE; keywords["div"] = T_DIV;
    keywords["mod"] = T_MOD; keywords["and"] = T_AND;
    keywords["or"] = T_OR; keywords["not"] = T_NOT;
    keywords["odd"] = T_ODD;
}

void Lexer::scan() {
    while (pos < src.length()) {
        char ch = src[pos];

        if (isspace(ch)) {
            if (ch == '\n') line++;
            pos++;
        } 
        else if (ch == '/' && pos + 1 < src.length() && src[pos+1] == '*') {
            // 注释处理
            pos += 2;
            while (pos + 1 < src.length() && !(src[pos] == '*' && src[pos+1] == '/')) {
                if (src[pos] == '\n') line++;
                pos++;
            }
            pos += 2; 
        }
        else if (isalpha(ch)) {
            // 标识符或关键字
            string buf;
            while (pos < src.length() && (isalnum(src[pos]) || src[pos] == '_')) buf += src[pos++];
            TokenType type = T_IDENT;
            if (keywords.count(buf)) type = keywords[buf];
            addToken(type, buf);
        }
        else if (isdigit(ch)) {
            // 数字 (处理 10.. vs 10.5)
            string buf;
            while (pos < src.length() && isdigit(src[pos])) buf += src[pos++];
            
            // 关键逻辑：如果是 '.', 检查下一个字符
            if (pos < src.length() && src[pos] == '.') {
                if (pos + 1 < src.length() && src[pos+1] == '.') {
                    // 是 .. ，当前数字是整数，不吃掉点
                } else {
                    // 是实数
                    buf += src[pos++];
                    while (pos < src.length() && isdigit(src[pos])) buf += src[pos++];
                }
            }
            addToken(T_NUMBER, buf);
        }
        else {
            // 符号
            string buf;
            buf += ch;
            pos++;
            
            if (ch == ':' && pos < src.length() && src[pos] == '=') { buf += "="; pos++; addToken(T_ASSIGN, buf); }
            else if (ch == '>' && pos < src.length() && src[pos] == '=') { buf += "="; pos++; addToken(T_GE, buf); }
            else if (ch == '<') {
                if (pos < src.length() && src[pos] == '=') { buf += "="; pos++; addToken(T_LE, buf); }
                else if (pos < src.length() && src[pos] == '>') { buf += ">"; pos++; addToken(T_NEQ, buf); }
                else addToken(T_LT, buf);
            }
            else if (ch == '.' && pos < src.length() && src[pos] == '.') { buf += "."; pos++; addToken(T_DOTDOT, buf); }
            else {
                switch(ch) {
                    case '+': addToken(T_PLUS, "+"); break;
                    case '-': addToken(T_MINUS, "-"); break;
                    case '*': addToken(T_MUL, "*"); break;
                    case '/': addToken(T_SLASH, "/"); break;
                    case '=': addToken(T_EQ, "="); break;
                    case '>': addToken(T_GT, ">"); break;
                    case '(': addToken(T_LPAREN, "("); break;
                    case ')': addToken(T_RPAREN, ")"); break;
                    case '[': addToken(T_LBRACKET, "["); break;
                    case ']': addToken(T_RBRACKET, "]"); break;
                    case ',': addToken(T_COMMA, ","); break;
                    case ';': addToken(T_SEMICOLON, ";"); break;
                    case '.': addToken(T_PERIOD, "."); break;
                    case ':': addToken(T_COLON, ":"); break;
                    default: 
                        // 错误：非法字符，暂略
                        // pos++; // Already incremented
                        break;
                }
            }
        }
    }
    addToken(T_EOF, "EOF");
}

void Lexer::addToken(TokenType type, string val) {
    Token t;
    t.id = ++tokenCount;
    t.line = line;
    t.type = type;
    t.value = val;
    if (type == T_NUMBER) {
        if (val.find('.') != string::npos) t.realVal = stod(val);
        else t.intVal = stoi(val);
    }
    tokens.push_back(t);
}
