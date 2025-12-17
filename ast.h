#ifndef AST_H
#define AST_H

#include "common.h"
#include <vector>
#include <string>

using namespace std;

struct ASTNode {
    int id;
    static int global_id;
    string type; // "Program", "Block", "IfStmt", "BinOp" etc.
    string info; // 额外信息，如操作符、变量名
    vector<ASTNode*> children;

    ASTNode(string t, string i = "") : type(t), info(i) { id = ++global_id; }
    virtual ~ASTNode() {
        for (auto c : children) delete c;
    }
    void add(ASTNode* node) { if(node) children.push_back(node); }
};

#endif
