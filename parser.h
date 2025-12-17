#ifndef PARSER_H
#define PARSER_H

#include "common.h"
#include "ast.h"
#include <vector>
#include <stack>
#include <string>

using namespace std;

class Parser {
    vector<Token>& tokens;
    int cur;
    vector<Error> errors;
    vector<Quadruple> quads;
    int tempCount;
    int labelCount;
    stack<string> loopExitLabels;

    Token peek();
    Token advance();
    void error(int code, string msg);
    bool match(TokenType t);
    void expect(TokenType t, int errCode);
    string newTemp();
    string newLabel();
    void emit(string op, string arg1, string arg2, string res);
    string parseIdentRef(string name);

    ASTNode* parseProgram();
    ASTNode* parseBlock();
    ASTNode* parseStmt();
    ASTNode* parseFactor();
    ASTNode* parseTerm();
    ASTNode* parseExp();

public:
    ASTNode* root;

    Parser(vector<Token>& t);
    ~Parser(); // Add destructor to clean up root
    void parse();
    vector<Error> getErrors();
    vector<Quadruple> getQuads();
};

#endif
