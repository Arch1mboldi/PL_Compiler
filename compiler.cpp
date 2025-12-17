#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <algorithm>
#include <sstream>
#include <iomanip>
#include <functional>
#include <stack>

using namespace std;

// ==========================================
// 1. 全局定义与枚举
// ==========================================

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
    string value; // 标识符存名字，数字存数值字符串
    int intVal;   // 预转换的整数值
    double realVal; // 预转换的实数值
};

struct Error {
    int code;
    int line;
    string msg;
};

// 三地址码结构
struct Quadruple {
    int id;
    string op;
    string arg1;
    string arg2;
    string result;
};

// ==========================================
// 2. 词法分析器 (Lexer)
// ==========================================

class Lexer {
private:
    string src;
    int pos;
    int line;
    int tokenCount;
    map<string, TokenType> keywords;

public:
    vector<Token> tokens;

    Lexer(string source) : src(source), pos(0), line(1), tokenCount(0) {
        initKeywords();
    }

    void initKeywords() {
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

    void scan() {
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
                while (isalnum(src[pos])) buf += src[pos++];
                TokenType type = T_IDENT;
                if (keywords.count(buf)) type = keywords[buf];
                addToken(type, buf);
            }
            else if (isdigit(ch)) {
                // 数字 (处理 10.. vs 10.5)
                string buf;
                while (isdigit(src[pos])) buf += src[pos++];
                
                // 关键逻辑：如果是 '.', 检查下一个字符
                if (src[pos] == '.') {
                    if (pos + 1 < src.length() && src[pos+1] == '.') {
                        // 是 .. ，当前数字是整数，不吃掉点
                    } else {
                        // 是实数
                        buf += src[pos++];
                        while (isdigit(src[pos])) buf += src[pos++];
                    }
                }
                addToken(T_NUMBER, buf);
            }
            else {
                // 符号
                string buf;
                buf += ch;
                pos++;
                
                if (ch == ':' && src[pos] == '=') { buf += "="; pos++; addToken(T_ASSIGN, buf); }
                else if (ch == '>' && src[pos] == '=') { buf += "="; pos++; addToken(T_GE, buf); }
                else if (ch == '<') {
                    if (src[pos] == '=') { buf += "="; pos++; addToken(T_LE, buf); }
                    else if (src[pos] == '>') { buf += ">"; pos++; addToken(T_NEQ, buf); }
                    else addToken(T_LT, buf);
                }
                else if (ch == '.' && src[pos] == '.') { buf += "."; pos++; addToken(T_DOTDOT, buf); }
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
                            break;
                    }
                }
            }
        }
        addToken(T_EOF, "EOF");
    }

    void addToken(TokenType type, string val) {
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
};

// ==========================================
// 3. 语法树节点 (AST)
// ==========================================

struct ASTNode {
    int id;
    static int global_id;
    string type; // "Program", "Block", "IfStmt", "BinOp" etc.
    string info; // 额外信息，如操作符、变量名
    vector<ASTNode*> children;

    ASTNode(string t, string i = "") : type(t), info(i) { id = ++global_id; }
    void add(ASTNode* node) { if(node) children.push_back(node); }
};
int ASTNode::global_id = 0;

// ==========================================
// 4. 语法分析与语义分析器
// ==========================================

class Parser {
    vector<Token>& tokens;
    int cur;
    vector<Error> errors;
    vector<Quadruple> quads;
    int tempCount;
    int labelCount;
    stack<string> loopExitLabels;

public:
    ASTNode* root;

    Parser(vector<Token>& t) : tokens(t), cur(0), tempCount(0), labelCount(0) {}

    Token peek() { return tokens[cur]; }
    Token advance() { return tokens[cur++]; }
    
    void error(int code, string msg) {
        errors.push_back({code, peek().line, msg});
        // 简单的错误恢复：跳过直到分号
        while(peek().type != T_SEMICOLON && peek().type != T_EOF && peek().type != T_END) {
            advance();
        }
    }

    bool match(TokenType t) {
        if (peek().type == t) {
            advance();
            return true;
        }
        return false;
    }

    void expect(TokenType t, int errCode) {
        if (!match(t)) {
            error(errCode, "Expected token type " + to_string(t));
        }
    }

    // 生成临时变量
    string newTemp() { return "t" + to_string(++tempCount); }
    // 生成标签
    string newLabel() { return "L" + to_string(++labelCount); }
    // 生成四元式
    void emit(string op, string arg1, string arg2, string res) {
        quads.push_back({(int)quads.size() + 1, op, arg1, arg2, res});
    }

    // --- 递归下降分析函数 ---

    void parse() {
        root = parseProgram();
    }
    ASTNode* parseProgram() {
        ASTNode* node = new ASTNode("Program");
        if (peek().type == T_PROGRAM) {
            advance(); // 消耗 "program" token
        }
        node->add(parseBlock());
        expect(T_PERIOD, 9); // 应为句号
        // 检查程序结束后是否还有多余的token
        if (peek().type != T_EOF) {
            error(47, "Unexpected tokens after program end.");
        }
        return node;
    }

    ASTNode* parseBlock() {
        ASTNode* node = new ASTNode("Block");
        
        // Const Decl
        if (peek().type == T_CONST) {
            advance();
            do {
                expect(T_IDENT, 4);
                expect(T_EQ, 1);
                expect(T_NUMBER, 2);
                expect(T_SEMICOLON, 5);
            } while (peek().type == T_IDENT); // 简化处理
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
        if (peek().type != T_END) { // 只要不是立即就 end，就解析语句
            node->add(parseStmt());
            while (match(T_SEMICOLON)) {
                if (peek().type == T_END) break; // 如果分号是多余的，后面直接跟了end，就跳出
                node->add(parseStmt());
            }
        }
        match(T_END);
        
        return node;
    }

    ASTNode* parseStmt() {
        ASTNode* node = new ASTNode("Stmt");
        
        if (peek().type == T_IDENT) {
            // Assignment or Call (if call is implicit? No, call is explicit keyword for proc)
            // But function return is ident := exp
            string id = peek().value;
            advance();
            // Array access?
            while (match(T_LBRACKET)) {
                parseExp();
                expect(T_RBRACKET, 22);
            }
            
            expect(T_ASSIGN, 13);
            string exprRes = parseExp()->info; // 获取表达式结果的临时变量名
            emit(":=", exprRes, "", id); // 生成赋值三地址码
            node->info = "Assign";
        }
        else if (match(T_IF)) {
            node->info = "If";
            string cond = parseExp()->info;
            string L_true = newLabel();
            string L_false = newLabel();
            
            emit("jnz", cond, "", L_true); // 如果为真跳转
            emit("j", "", "", L_false);
            
            emit("label", "", "", L_true);
            expect(T_THEN, 16);
            node->add(parseStmt());
            
            if (match(T_ELSE)) {
                string L_end = newLabel();
                emit("j", "", "", L_end); // 跳过else部分
                emit("label", "", "", L_false);
                node->add(parseStmt());
                emit("label", "", "", L_end);
            } else {
                emit("label", "", "", L_false);
            }
        }
        else if (match(T_WHILE)) {
            node = new ASTNode("Stmt", "While");
            advance(); // 消耗 'while'

            // 1. 创建循环用的标签
            string startLabel = newLabel(); // 循环开始的标签 (e.g., L4)
            string endLabel = newLabel();   // 循环结束/退出的标签 (e.g., L5)

            // 2. 将出口标签压入栈中，供 'exit' 语句使用
            loopExitLabels.push(endLabel);

            // 3. 生成循环开始的标签
            emit("label", "", "", startLabel);

            // 4. 解析循环条件
            ASTNode* cond = parseExp();
            node->add(cond);
            string condPlace = cond->info;

            // 5. 生成条件判断和跳转指令
            // 如果条件为假(0)，则跳转到循环出口
            emit("jz", condPlace, "", endLabel);

            // 6. 解析循环体
            expect(T_DO, 26);
            node->add(parseStmt()); // 递归调用 parseStmt 解析循环体

            // 7. 生成无条件跳转，回到循环开始处进行下一次判断
            emit("j", "", "", startLabel);

            // 8. 生成循环出口标签
            emit("label", "", "", endLabel);

            // 9. 循环解析完毕，将出口标签从栈中弹出
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
                string t = parseExp()->info;
                emit("write", "", "", t);
            } while (match(T_COMMA));
            expect(T_RPAREN, 34);
        }
        else if (match(T_EXIT)) {
            node = new ASTNode("Stmt", "Exit");
            advance(); // 消耗 'exit' token

            if (loopExitLabels.empty()) {
                error(0, "EXIT statement not within a loop.");
            } else {
                // 生成跳转指令，跳转到当前循环的出口标签
                emit("j", "", "", loopExitLabels.top());
            }
        }
        
        return node;
    }

    // 新增辅助函数：解析标识符引用（变量、数组、函数返回）
    // IdentRef -> ident [ ‘[’Exp‘]’ { ‘[’Exp‘]’ } ]
    // 这个函数处理变量名和可能的数组下标
    string parseIdentRef(string name) {
        string current_ref = name;
        while (match(T_LBRACKET)) {
            ASTNode* indexExpr = parseExp();
            string temp_base = current_ref; // 保存当前的基地址/变量名
            string temp_index = indexExpr->info; // 下标表达式的结果
            string new_ref = newTemp(); // 新的临时变量存储地址或值
            
            // 生成计算数组元素地址的TAC
            // 这需要符号表信息来获取数组的基地址和元素大小
            // 这里简化为伪指令 "array_access"
            emit("array_access", temp_base, temp_index, new_ref);

            current_ref = new_ref;
            expect(T_RBRACKET, 22);
        }
        return current_ref;
    }


    // 新增的、完整的 parseFactor 函数
    ASTNode* parseFactor() {
        ASTNode* node = new ASTNode("Factor");
        string place; // 用于存储此因子结果所在的临时变量或字面量

        switch (peek().type) {
            case T_IDENT: {
                string name = peek().value;
                advance(); // 消耗标识符

                // 可能是函数调用，也可能是变量/数组
                if (peek().type == T_LPAREN) { // 函数调用: ident ( ActParal )
                    node->type = "FuncCall";
                    node->info = name;
                    
                    advance(); // 消耗 '('
                    
                    vector<string> args;
                    if (peek().type != T_RPAREN) {
                        do {
                            ASTNode* argExpr = parseExp();
                            args.push_back(argExpr->info);
                        } while (match(T_COMMA));
                    }
                    expect(T_RPAREN, 22);

                    // 生成参数和调用的TAC
                    for (const string& arg : args) {
                        emit("param", "", "", arg);
                    }
                    place = newTemp();
                    emit("call", name, to_string(args.size()), place);

                } else { // 变量或数组引用: IdentRef
                    node->type = "IdentRef";
                    node->info = name;
                    // 调用辅助函数处理可能的数组下标
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
                node = parseExp(); // 递归调用 parseExp
                place = node->info; // 结果就是内部表达式的结果
                expect(T_RPAREN, 22);
                break;

            case T_NOT:
                match(T_NOT);
                node = parseFactor(); // not Factor
                place = newTemp();
                emit("not", node->info, "", place);
                break;
                
            case T_ODD:
                match(T_ODD);
                expect(T_LPAREN, 34);
                node = parseExp(); // odd (SimpExp) - 我们用 parseExp 简化
                place = newTemp();
                emit("odd", node->info, "", place);
                expect(T_RPAREN, 34);
                break;

            case T_TRUE:
                place = "1"; // 通常用 1 代表 true
                node->info = "true";
                advance();
                emit("=", "1", "", place); // 或者直接 t_x := 1
                break;

            case T_FALSE:
                place = "0"; // 通常用 0 代表 false
                node->info = "false";
                advance();
                emit("=", "0", "", place); // 或者直接 t_x := 0
                break;
            
            case T_EXIT: {
                node = new ASTNode("Stmt", "Exit");
                advance(); // 消耗 'exit' token

                // TAC Generation for exit
                // 'exit' 需要跳转到当前循环的出口标签
                // 我们需要一个机制来获取这个标签
                if (loopExitLabels.empty()) {
                    error(0, "EXIT statement not within a loop.");
                } else {
                    emit("j", "", "", loopExitLabels.top());
                }
                break;
            }

            default:
                error(24, "Expression cannot start with this token: " + peek().value);
                // 错误恢复：跳过一个token，尝试继续
                advance();
                place = newTemp(); // 返回一个空临时变量
                break;
        }
        
        node->info = place; // 将结果的"名字"存入节点
        return node;
    }


    // 替换旧的 parseTerm
    ASTNode* parseTerm() {
        ASTNode* node = new ASTNode("Term");
        
        ASTNode* left = parseFactor();
        string place = left->info;
        
        // 处理乘法级运算符
        while (peek().type == T_MUL || peek().type == T_SLASH || 
            peek().type == T_DIV || peek().type == T_MOD ||
            peek().type == T_AND) {
            
            string op = peek().value;
            TokenType op_type = peek().type;
            advance();
            
            // 创建一个二元操作节点（用于AST）
            ASTNode* opNode = new ASTNode("BinOp", op);
            opNode->add(left); // 左子节点是之前的因子

            ASTNode* right = parseFactor();
            opNode->add(right);
            
            // 生成TAC
            string newPlace = newTemp();
            emit(op, place, right->info, newPlace);
            
            place = newPlace; // 更新当前结果位置
            left = opNode; // 更新AST的左侧，以处理 a*b*c 的情况
        }
        
        node->add(left);
        node->info = place;
        return node;
    }


    // 同样，提供一个增强版的 parseExp 来协同工作
    // Exp -> SimpExp [RelOp SimpExp]
    // SimpExp -> [+|-] Term {AddOp Term}
    ASTNode* parseExp() {
        ASTNode* node = new ASTNode("Expression");

        // 首先处理可能的 SimpExp 开头的 [+|-]
        bool is_unary_minus = false;
        if (peek().type == T_MINUS) {
            is_unary_minus = true;
            advance();
        } else if (peek().type == T_PLUS) {
            advance();
        }

        ASTNode* left = parseTerm();
        string place = left->info;

        // 如果有前导负号，生成TAC
        if (is_unary_minus) {
            string temp = newTemp();
            emit("uminus", place, "", temp); // uminus: 一元负号
            place = temp;
        }
        
        // 处理加法级运算符
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

        // 处理关系运算符
        if (peek().type == T_EQ || peek().type == T_NEQ || peek().type == T_LT ||
            peek().type == T_LE || peek().type == T_GT || peek().type == T_GE) {
            
            string op = peek().value;
            advance();

            ASTNode* opNode = new ASTNode("RelOp", op);
            opNode->add(left);

            ASTNode* right = parseExp(); // 关系运算符右边可以是完整的 SimpExp
            opNode->add(right);

            string newPlace = newTemp();
            
            // 生成跳转指令，而不是直接计算布尔值（更高效）
            // 这里为了简化，我们还是生成布尔值 0 或 1
            // (j, op, arg1, arg2, label) 是更常见的做法
            emit(op, place, right->info, newPlace);
            
            place = newPlace;
            left = opNode;
        }
        
        node->add(left);
        node->info = place;
        return node;
    }


    

    // 获取数据以便输出
    vector<Error> getErrors() { return errors; }
    vector<Quadruple> getQuads() { return quads; }
};


// ==========================================
// 5. JSON 输出与 主程序
// ==========================================

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
