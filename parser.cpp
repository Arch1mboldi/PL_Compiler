#include "parser.h"
#include <iostream>

using namespace std;

Parser::Parser(vector<Token>& t) : tokens(t), cur(0), tempCount(0), labelCount(0), root(nullptr) {}

Parser::~Parser() {
    if (root) delete root;
}

Token Parser::peek() { 
    if (cur >= tokens.size()) {
        return {0, 0, T_EOF, "EOF"};
    }
    return tokens[cur]; 
}

Token Parser::advance() { 
    if (cur < tokens.size()) {
        return tokens[cur++];
    }
    return {0, 0, T_EOF, "EOF"};
}

void Parser::error(int code, string msg) {
    errors.push_back({code, peek().line, msg});
    // 简单的错误恢复：跳过直到分号
    while(peek().type != T_SEMICOLON && peek().type != T_EOF && peek().type != T_END) {
        advance();
    }
}

bool Parser::match(TokenType t) {
    if (peek().type == t) {
        advance();
        return true;
    }
    return false;
}

void Parser::expect(TokenType t, int errCode) {
    if (!match(t)) {
        error(errCode, "Expected token type " + to_string(t));
    }
}

string Parser::newTemp() { return "t" + to_string(++tempCount); }
string Parser::newLabel() { return "L" + to_string(++labelCount); }
void Parser::emit(string op, string arg1, string arg2, string res) {
    quads.push_back({(int)quads.size() + 1, op, arg1, arg2, res});
}

void Parser::parse() {
    root = parseProgram();
}

ASTNode* Parser::parseProgram() {
    ASTNode* node = new ASTNode("Program");
    if (peek().type == T_PROGRAM) {
        advance(); 
    }
    node->add(parseBlock());
    expect(T_PERIOD, 9); 
    
    if (peek().type != T_EOF) {
        error(47, "Unexpected tokens after program end.");
    }
    return node;
}

ASTNode* Parser::parseBlock() {
    ASTNode* node = new ASTNode("Block");
    
    // Const Decl
    if (peek().type == T_CONST) {
        advance();
        do {
            expect(T_IDENT, 4);
            expect(T_EQ, 1);
            expect(T_NUMBER, 2);
            expect(T_SEMICOLON, 5);
        } while (peek().type == T_IDENT); 
        node->add(new ASTNode("ConstDecls"));
    }

    // Type Decl
    if (peek().type == T_TYPE) {
        advance();
        while (peek().type == T_IDENT) {
            advance(); // ident
            expect(T_EQ, 3);
            // TypeExp 解析 (简化：假设只有 array)
            if (match(T_ARRAY)) {
                expect(T_LBRACKET, 29);
                expect(T_NUMBER, 2);
                expect(T_DOTDOT, 29);
                expect(T_NUMBER, 2);
                expect(T_RBRACKET, 22);
                expect(T_OF, 29);
                // Type
                advance(); 
            } else {
                advance(); // 基础类型
            }
            expect(T_SEMICOLON, 5);
        }
        node->add(new ASTNode("TypeDecls"));
    }

    // Var Decl
    if (peek().type == T_VAR) {
        advance();
        while (peek().type == T_IDENT) {
            do {
                advance();
            } while (match(T_COMMA));
            expect(T_COLON, 5);
            advance(); // Type
            expect(T_SEMICOLON, 5);
        }
        node->add(new ASTNode("VarDecls"));
    }

    // Func/Proc Decl
    while (peek().type == T_PROCEDURE || peek().type == T_FUNCTION) {
        bool isFunc = (peek().type == T_FUNCTION);
        ASTNode* sub = new ASTNode(isFunc ? "FuncDecl" : "ProcDecl");
        advance();
        string name = peek().value;
        advance(); // ident
        
        if (match(T_LPAREN)) {
            // 参数列表解析 (略)
            while(peek().type != T_RPAREN) advance();
            match(T_RPAREN);
        }
        
        if (isFunc) {
            expect(T_COLON, 5);
            advance(); // 返回类型
        }
        expect(T_SEMICOLON, 5);
        sub->add(parseBlock());
        expect(T_SEMICOLON, 5);
        node->add(sub);
    }

    expect(T_BEGIN, 48);
    if (peek().type != T_END) { 
        node->add(parseStmt());
        while (match(T_SEMICOLON)) {
            if (peek().type == T_END) break; 
            node->add(parseStmt());
        }
    }
    match(T_END);
    
    return node;
}

ASTNode* Parser::parseStmt() {
    ASTNode* node = new ASTNode("Stmt");
    
    if (peek().type == T_IDENT) {
        // Assignment or Call 
        string id = peek().value;
        advance();
        // Array access?
        // Note: Check for T_ASSIGN first to avoid consuming LBRACKET if not assignment
        // Wait, original code parsed array access here.
        // IdentRef logic embedded here in original code for assignment target.
        
        string current_target = id;
        while (match(T_LBRACKET)) {
            ASTNode* indexExpr = parseExp();
            string temp_base = current_target;
            string temp_index = indexExpr->info;
            string new_ref = newTemp();
            emit("array_addr", temp_base, temp_index, new_ref); // Changed to array_addr for LHS
            current_target = new_ref; // Use the address
            expect(T_RBRACKET, 22);
        }
        
        expect(T_ASSIGN, 13);
        ASTNode* expr = parseExp();
        string exprRes = expr->info; 
        
        // If current_target is a temp (from array access), it holds an address.
        // We need a store instruction or just := if it's a simple var.
        // The original code used := for both, but for array it used a temp.
        // emit(":=", exprRes, "", id); // Original
        
        // Corrected logic for assignment
        emit(":=", exprRes, "", current_target); 
        node->info = "Assign";
    }
    else if (match(T_IF)) {
        node->info = "If";
        ASTNode* condNode = parseExp();
        string cond = condNode->info;
        string L_true = newLabel();
        string L_false = newLabel();
        
        emit("jnz", cond, "", L_true); 
        emit("j", "", "", L_false);
        
        emit("label", "", "", L_true);
        expect(T_THEN, 16);
        node->add(parseStmt());
        
        if (match(T_ELSE)) {
            string L_end = newLabel();
            emit("j", "", "", L_end); 
            emit("label", "", "", L_false);
            node->add(parseStmt());
            emit("label", "", "", L_end);
        } else {
            emit("label", "", "", L_false);
        }
    }
    else if (match(T_WHILE)) {
        node = new ASTNode("Stmt", "While");
        // 'while' already consumed by match

        string startLabel = newLabel(); 
        string endLabel = newLabel();   

        loopExitLabels.push(endLabel);

        emit("label", "", "", startLabel);

        ASTNode* cond = parseExp();
        node->add(cond);
        string condPlace = cond->info;

        emit("jz", condPlace, "", endLabel);

        expect(T_DO, 26);
        node->add(parseStmt()); 

        emit("j", "", "", startLabel);

        emit("label", "", "", endLabel);

        loopExitLabels.pop();

    }
    else if (match(T_BEGIN)) {
        node->info = "Compound";
        node->add(parseStmt());
        while (match(T_SEMICOLON)) {
            node->add(parseStmt());
        }
        expect(T_END, 17);
    }
    else if (match(T_WRITE)) {
        node->info = "Write";
        expect(T_LPAREN, 34);
        do {
            ASTNode* exp = parseExp();
            string t = exp->info;
            emit("write", "", "", t);
        } while (match(T_COMMA));
        expect(T_RPAREN, 34);
    }
    else if (match(T_EXIT)) {
        node = new ASTNode("Stmt", "Exit");
        // 'exit' already consumed

        if (loopExitLabels.empty()) {
            error(0, "EXIT statement not within a loop.");
        } else {
            emit("j", "", "", loopExitLabels.top());
        }
    }
    else if (match(T_CALL)) {
        // Handle CALL statement explicitly as it's in the grammar/keywords
        node->info = "Call";
        if (peek().type == T_IDENT) {
            string name = peek().value;
            advance();
            // Check for parameters if any (simplified)
            emit("call", name, "0", ""); // Assuming 0 args for now or no args parsed
        } else {
             error(14, "Expected identifier after CALL");
        }
    }
    
    return node;
}

string Parser::parseIdentRef(string name) {
    string current_ref = name;
    while (match(T_LBRACKET)) {
        ASTNode* indexExpr = parseExp();
        string temp_base = current_ref; 
        string temp_index = indexExpr->info; 
        string new_ref = newTemp(); 
        
        emit("array_access", temp_base, temp_index, new_ref);

        current_ref = new_ref;
        expect(T_RBRACKET, 22);
    }
    return current_ref;
}

ASTNode* Parser::parseFactor() {
    ASTNode* node = new ASTNode("Factor");
    string place; 

    switch (peek().type) {
        case T_IDENT: {
            string name = peek().value;
            advance(); 

            if (peek().type == T_LPAREN) { 
                node->type = "FuncCall";
                node->info = name;
                
                advance(); 
                
                vector<string> args;
                if (peek().type != T_RPAREN) {
                    do {
                        ASTNode* argExpr = parseExp();
                        args.push_back(argExpr->info);
                    } while (match(T_COMMA));
                }
                expect(T_RPAREN, 22);

                for (const string& arg : args) {
                    emit("param", "", "", arg);
                }
                place = newTemp();
                emit("call", name, to_string(args.size()), place);

            } else { 
                node->type = "IdentRef";
                node->info = name;
                place = parseIdentRef(name);
            }
            break;
        }

        case T_NUMBER:
            place = peek().value;
            node->info = place;
            advance();
            break;

        case T_LPAREN:
            match(T_LPAREN);
            delete node; // Avoid leak
            node = parseExp(); 
            place = node->info; 
            expect(T_RPAREN, 22);
            break;

        case T_NOT:
            match(T_NOT);
            delete node; // Avoid leak
            node = parseFactor(); 
            place = newTemp();
            emit("not", node->info, "", place);
            break;
            
        case T_ODD:
            match(T_ODD);
            expect(T_LPAREN, 34);
            delete node; // Avoid leak
            node = parseExp(); 
            place = newTemp();
            emit("odd", node->info, "", place);
            expect(T_RPAREN, 34);
            break;

        case T_TRUE:
            place = "1"; 
            node->info = "true";
            advance();
            // emit("=", "1", "", place); // Optimization: Use literal directly
            break;

        case T_FALSE:
            place = "0"; 
            node->info = "false";
            advance();
            // emit("=", "0", "", place); 
            break;
        
        // EXIT is a statement, not a factor, but sometimes parser confusion happens.
        // It's handled in parseStmt.

        default:
            error(24, "Expression cannot start with this token: " + peek().value);
            advance();
            place = newTemp(); 
            break;
    }
    
    node->info = place; 
    return node;
}

ASTNode* Parser::parseTerm() {
    ASTNode* node = new ASTNode("Term");
    
    ASTNode* left = parseFactor();
    string place = left->info;
    
    while (peek().type == T_MUL || peek().type == T_SLASH || 
        peek().type == T_DIV || peek().type == T_MOD ||
        peek().type == T_AND) {
        
        string op = peek().value;
        advance();
        
        ASTNode* opNode = new ASTNode("BinOp", op);
        opNode->add(left); 

        ASTNode* right = parseFactor();
        opNode->add(right);
        
        string newPlace = newTemp();
        emit(op, place, right->info, newPlace);
        
        place = newPlace; 
        left = opNode; 
    }
    
    if (left != node) {
         // If we created BinOps, 'left' is the top of that tree.
         // But we started with 'node' as "Term".
         // We should attach 'left' to 'node' or just return 'left' if 'node' is empty wrapper.
         // Current logic: node->add(left)
         node->add(left);
    } else {
        // If no loop, left is the factor.
         node->add(left);
    }
    // Re-verify the loop logic.
    // Initial: node created. left parsed. Loop runs, updates left.
    // Finally, add left to node.
    // Actually, if left became a tree of BinOps, we add that tree to Term node.
    
    node->info = place;
    return node;
}

ASTNode* Parser::parseExp() {
    ASTNode* node = new ASTNode("Expression");

    bool is_unary_minus = false;
    if (peek().type == T_MINUS) {
        is_unary_minus = true;
        advance();
    } else if (peek().type == T_PLUS) {
        advance();
    }

    ASTNode* left = parseTerm();
    string place = left->info;

    if (is_unary_minus) {
        string temp = newTemp();
        emit("uminus", place, "", temp); 
        place = temp;
    }
    
    while (peek().type == T_PLUS || peek().type == T_MINUS || peek().type == T_OR) {
        string op = peek().value;
        advance();

        ASTNode* opNode = new ASTNode("BinOp", op);
        opNode->add(left);

        ASTNode* right = parseTerm();
        opNode->add(right);
        
        string newPlace = newTemp();
        emit(op, place, right->info, newPlace);
        
        place = newPlace;
        left = opNode;
    }

    if (peek().type == T_EQ || peek().type == T_NEQ || peek().type == T_LT ||
        peek().type == T_LE || peek().type == T_GT || peek().type == T_GE) {
        
        string op = peek().value;
        advance();

        ASTNode* opNode = new ASTNode("RelOp", op);
        opNode->add(left);

        ASTNode* right = parseExp(); // Right side of relation
        opNode->add(right);

        string newPlace = newTemp();
        
        emit(op, place, right->info, newPlace);
        
        place = newPlace;
        left = opNode;
    }
    
    node->add(left);
    node->info = place;
    return node;
}

vector<Error> Parser::getErrors() { return errors; }
vector<Quadruple> Parser::getQuads() { return quads; }
