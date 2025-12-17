#ifndef LEXER_H
#define LEXER_H

#include "common.h"
#include <string>
#include <vector>
#include <map>

using namespace std;

class Lexer {
private:
    string src;
    int pos;
    int line;
    int tokenCount;
    map<string, TokenType> keywords;

    void initKeywords();
    void addToken(TokenType type, string val);

public:
    vector<Token> tokens;

    Lexer(string source);
    void scan();
};

#endif
