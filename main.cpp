#include "lexer.h"
#include "parser.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include <functional>

using namespace std;

// Define static member
int ASTNode::global_id = 0;

void dumpJSON(const vector<Token>& tokens, ASTNode* ast, const vector<Quadruple>& quads, const vector<Error>& errs) {
    ofstream out("output.json");
    out << "{\n";
    
    // Tokens
    out << "  \"tokens\": [\n";
    for(size_t i=0; i<tokens.size(); ++i) {
        out << "    {\"id\": " << tokens[i].id << ", \"line\": " << tokens[i].line 
            << ", \"type\": " << tokens[i].type << ", \"value\": \"" << tokens[i].value << "\"}" 
            << (i < tokens.size()-1 ? "," : "") << "\n";
    }
    out << "  ],\n";

    // Errors
    out << "  \"errors\": [\n";
    for(size_t i=0; i<errs.size(); ++i) {
        out << "    {\"code\": " << errs[i].code << ", \"line\": " << errs[i].line 
            << ", \"msg\": \"" << errs[i].msg << "\"}" 
            << (i < errs.size()-1 ? "," : "") << "\n";
    }
    out << "  ],\n";

    // TAC
    out << "  \"tac\": [\n";
    for(size_t i=0; i<quads.size(); ++i) {
        out << "    {\"id\": " << quads[i].id << ", \"op\": \"" << quads[i].op 
            << "\", \"arg1\": \"" << quads[i].arg1 << "\", \"arg2\": \"" << quads[i].arg2 
            << "\", \"result\": \"" << quads[i].result << "\"}" 
            << (i < quads.size()-1 ? "," : "") << "\n";
    }
    out << "  ],\n";

    // AST (Simple recursive dump)
    out << "  \"ast\": ";
    function<void(ASTNode*)> printNode = [&](ASTNode* node) {
        if (!node) { out << "null"; return; }
        out << "{\"id\": " << node->id << ", \"type\": \"" << node->type << "\", \"info\": \"" << node->info << "\", \"children\": [";
        for(size_t i=0; i<node->children.size(); ++i) {
            printNode(node->children[i]);
            if(i < node->children.size()-1) out << ",";
        }
        out << "]}";
    };
    printNode(ast);
    out << "\n}";
    out.close();
    cout << "Compilation finished. Result saved to output.json" << endl;
}

int main() {
    // 读取 test.pl
    ifstream file("test.pl");
    if (!file.is_open()) {
        cerr << "Error: Cannot open test.pl" << endl;
        return 1;
    }
    stringstream buffer;
    buffer << file.rdbuf();
    string source = buffer.str();

    // 1. 词法分析
    Lexer lexer(source);
    lexer.scan();

    // 2. 语法分析 & 中间代码生成
    Parser parser(lexer.tokens);
    parser.parse();

    // 3. 输出 JSON
    dumpJSON(lexer.tokens, parser.root, parser.getQuads(), parser.getErrors());

    return 0;
}
