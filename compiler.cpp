// 编译器主程序 - 实现了一个类Pascal语言的编译器
// 包含词法分析、语法分析、语义分析和中间代码生成功能

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
#include <cctype>

using namespace std;

// 词法单元类型枚举 - 定义了编译器识别的所有token类型
// 这是词法分析阶段的输出，语法分析阶段的输入
enum TokenType {
    T_NULL,        // 空token
    T_IDENT,       // 标识符
    T_NUMBER,      // 数字常量
    T_STRING,      // 字符串常量
    T_PROGRAM,     // program关键字
    T_CONST,       // const关键字
    T_VAR,         // var关键字
    T_PROCEDURE,   // procedure关键字
    T_FUNCTION,    // function关键字
    T_BEGIN,       // begin关键字
    T_END,         // end关键字
    T_IF,          // if关键字
    T_THEN,        // then关键字
    T_ELSE,        // else关键字
    T_WHILE,       // while关键字
    T_DO,          // do关键字
    T_CALL,        // call关键字
    T_READ,        // read关键字
    T_WRITE,       // write关键字
    T_EXIT,        // exit关键字
    T_ODD,         // odd关键字
    T_TYPE,        // type关键字
    T_ARRAY,       // array关键字
    T_OF,          // of关键字
    T_OR,          // or运算符
    T_AND,         // and运算符
    T_NOT,         // not运算符
    T_DIV,         // div运算符（整数除法）
    T_MOD,         // mod运算符（取模）
    T_TRUE,        // true关键字
    T_FALSE,       // false关键字
    T_INTEGER,     // integer类型
    T_REAL,        // real类型
    T_BOOLEAN,     // boolean类型
    T_PLUS,        // 加号
    T_MINUS,       // 减号
    T_MUL,         // 乘号
    T_SLASH,       // 斜杠
    T_EQ,          // 等于
    T_NEQ,         // 不等于
    T_LT,          // 小于
    T_LE,          // 小于等于
    T_GT,          // 大于
    T_GE,          // 大于等于
    T_LPAREN,      // 左圆括号
    T_RPAREN,      // 右圆括号
    T_LBRACKET,    // 左方括号
    T_RBRACKET,    // 右方括号
    T_COMMA,       // 逗号
    T_SEMICOLON,   // 分号
    T_PERIOD,      // 句号
    T_ASSIGN,      // 赋值符号
    T_COLON,       // 冒号
    T_DOTDOT,      // 双点（用于数组范围）
    T_EOF          // 文件结束
};

// 词法单元结构体 - 词法分析器的基本输出单位
// 包含了token的所有必要信息，用于后续的语法分析
struct Token {
    int id = 0;              // token的唯一标识符
    int line = 0;            // token所在源代码的行号
    TokenType type = T_NULL; // token类型
    string value;           // token的字面值
    int intVal = 0;         // 整数值（当token为数字时使用）
    double realVal = 0.0;   // 实数值（当token为实数时使用）
};

// 错误信息结构体 - 用于记录编译过程中的错误
struct Error {
    int code;      // 错误代码
    int line;      // 错误所在行号
    string msg;    // 错误信息描述
};

// 四元式结构体 - 中间代码表示形式
// 四元式是编译原理中常用的中间代码表示方法：(op, arg1, arg2, result)
struct Quadruple {
    int id;        // 四元式的序号
    string op;     // 操作符
    string arg1;   // 第一个操作数
    string arg2;   // 第二个操作数
    string result; // 结果
};

// 词法分析器类 - 编译器的前端，负责将源代码转换为token序列
// 实现了编译原理中的词法分析阶段，使用有限状态自动机的思想
class Lexer {
private:
    string src;                    // 源代码字符串
    size_t pos;                    // 当前扫描位置
    int line;                      // 当前行号
    int tokenCount;                // token计数器
    map<string, TokenType> keywords; // 关键字映射表

public:
    vector<Token> tokens;          // 输出的token序列

    // 构造函数 - 初始化词法分析器
    Lexer(string source) : src(move(source)), pos(0), line(1), tokenCount(0) {
        initKeywords();           // 初始化关键字表
    }

    // 初始化关键字映射表 - 将字符串关键字映射为TokenType枚举值
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

    // 词法扫描主方法 - 实现有限状态自动机，逐个字符扫描源代码
    // 根据字符的不同类型进入不同的处理分支，生成相应的token
    void scan() {
        while (pos < src.length()) {
            char ch = src[pos];
            // 处理空白字符（空格、制表符、换行等）
            if (isspace(static_cast<unsigned char>(ch))) {
                if (ch == '\n') line++;  // 换行时增加行号
                pos++;
            } 
            // 处理块注释 (* ... *)
            else if (ch == '/' && pos + 1 < src.length() && src[pos + 1] == '*') {
                pos += 2;  // 跳过 /*
                while (pos + 1 < src.length() && !(src[pos] == '*' && src[pos + 1] == '/')) {
                    if (src[pos] == '\n') line++;
                    pos++;
                }
                if (pos + 1 < src.length()) pos += 2;  // 跳过 */
            } 
            // 处理标识符和关键字
            else if (isalpha(static_cast<unsigned char>(ch))) {
                string buf;
                // 读取连续的字母数字字符组成标识符
                while (pos < src.length() && isalnum(static_cast<unsigned char>(src[pos]))) {
                    buf += src[pos++];
                }
                TokenType type = T_IDENT;
                string lowerBuf = buf;
                // 转换为小写进行关键字匹配
                transform(lowerBuf.begin(), lowerBuf.end(), lowerBuf.begin(), ::tolower);
                if (keywords.count(lowerBuf)) {
                    type = keywords[lowerBuf];  // 如果是关键字，更新token类型
                    buf = lowerBuf;
                }
                addToken(type, buf);
            } 
            // 处理数字常量（整数和实数）
            else if (isdigit(static_cast<unsigned char>(ch))) {
                string buf;
                bool isReal = false;
                // 读取整数部分
                while (pos < src.length() && isdigit(static_cast<unsigned char>(src[pos]))) {
                    buf += src[pos++];
                }
                // 处理小数部分
                if (pos < src.length() && src[pos] == '.') {
                    if (pos + 1 < src.length() && src[pos + 1] == '.') {
                        // 如果是 ..，则不是小数点，不处理
                    } else {
                        isReal = true;
                        buf += src[pos++];
                        // 读取小数部分
                        while (pos < src.length() && isdigit(static_cast<unsigned char>(src[pos]))) {
                            buf += src[pos++];
                        }
                    }
                }
                addToken(T_NUMBER, buf);
                tokens.back().realVal = stod(buf);  // 设置实数值
                if (!isReal) tokens.back().intVal = stoi(buf);  // 设置整数值
            } 
            // 处理字符串常量
            else if (ch == '\'') {
                string buf;
                pos++;  // 跳过开始的引号
                // 读取字符串内容直到遇到结束引号
                while (pos < src.length() && src[pos] != '\'') {
                    if (src[pos] == '\n') line++;
                    buf += src[pos++];
                }
                if (pos < src.length()) pos++;  // 跳过结束的引号
                addToken(T_STRING, buf);
            } 
            // 处理运算符和分隔符
            else {
                pos++;
                switch (ch) {
                    case ':':  // 赋值符号 := 或冒号
                        if (pos < src.length() && src[pos] == '=') {
                            pos++;
                            addToken(T_ASSIGN, ":=");
                        } else addToken(T_COLON, ":");
                        break;
                    case '<':  // 小于运算符系列
                        if (pos < src.length() && src[pos] == '=') {
                            pos++;
                            addToken(T_LE, "<=");
                        } else if (pos < src.length() && src[pos] == '>') {
                            pos++;
                            addToken(T_NEQ, "<>");
                        } else addToken(T_LT, "<");
                        break;
                    case '>':  // 大于运算符系列
                        if (pos < src.length() && src[pos] == '=') {
                            pos++;
                            addToken(T_GE, ">=");
                        } else addToken(T_GT, ">");
                        break;
                    case '.':  // 双点运算符或句号
                        if (pos < src.length() && src[pos] == '.') {
                            pos++;
                            addToken(T_DOTDOT, "..");
                        } else addToken(T_PERIOD, ".");
                        break;
                    case '+': addToken(T_PLUS, "+"); break;
                    case '-': addToken(T_MINUS, "-"); break;
                    case '*': addToken(T_MUL, "*"); break;
                    case '/': addToken(T_SLASH, "/"); break;
                    case '=': addToken(T_EQ, "="); break;
                    case '(': addToken(T_LPAREN, "("); break;
                    case ')': addToken(T_RPAREN, ")"); break;
                    case '[': addToken(T_LBRACKET, "["); break;
                    case ']': addToken(T_RBRACKET, "]"); break;
                    case ',': addToken(T_COMMA, ","); break;
                    case ';': addToken(T_SEMICOLON, ";"); break;
                    default: break;  // 忽略无法识别的字符
                }
            }
        }
        addToken(T_EOF, "EOF");  // 添加文件结束标记
    }

    // 添加token到token序列 - 构造Token对象并添加到tokens向量中
    void addToken(TokenType type, const string& val) {
        Token t;
        t.id = ++tokenCount;  // 分配唯一的token ID
        t.line = line;        // 记录行号
        t.type = type;        // 设置token类型
        t.value = val;        // 设置token值
        tokens.push_back(t);   // 添加到token序列
    }
};

// 数据类型枚举 - 定义了编译器支持的所有数据类型
// 用于语义分析阶段的类型检查
enum DataType { 
    D_UNKNOWN,  // 未知类型
    D_INTEGER,  // 整型
    D_REAL,     // 实型
    D_BOOLEAN,  // 布尔型
    D_STRING,   // 字符串型
    D_VOID      // 无类型（用于过程）
};

// 符号表信息结构体 - 存储标识符的语义信息
// 用于语义分析阶段的符号管理和类型检查
struct SymbolInfo {
    DataType valueType = D_UNKNOWN;  // 标识符的值类型
    bool isArray = false;            // 是否为数组
    DataType elementType = D_UNKNOWN; // 数组元素类型
};

// 类型信息结构体 - 存储类型定义信息
// 用于自定义类型和数组类型的表示
struct TypeInfo {
    bool isArray = false;           // 是否为数组类型
    DataType baseType = D_UNKNOWN;  // 基础类型
};

// 抽象语法树节点结构体 - 语法分析的输出表示
// 使用树形结构表示程序的语法结构，便于后续的语义分析和代码生成
struct ASTNode {
    int id;                        // 节点唯一标识符
    static int global_id;           // 全局节点计数器
    string type;                   // 节点类型（如"Program"、"Block"、"Stmt"等）
    string info;                   // 节点附加信息（如操作符、标识符名等）
    DataType dataType = D_UNKNOWN; // 节点的数据类型
    vector<ASTNode*> children;     // 子节点列表
    
    // 构造函数 - 初始化AST节点
    ASTNode(string t, string i = "") : type(move(t)), info(move(i)) { id = ++global_id; }
    
    // 添加子节点 - 构建语法树结构
    void add(ASTNode* node) { if (node) children.push_back(node); }
};
// 静态成员初始化
int ASTNode::global_id = 0;

// 语法分析器类 - 编译器的核心组件，实现语法分析、语义分析和中间代码生成
// 使用递归下降分析法，同时进行语义检查和四元式生成
class Parser {
    // 设计符结构体 - 表示变量标识符及其数组索引
    struct Designator {
        string name;                    // 标识符名称
        vector<string> indices;         // 数组索引列表
        SymbolInfo symbol;              // 符号表信息
        int line;                      // 所在行号
    };

    vector<Token>& tokens;             // token序列引用
    int cur;                          // 当前token位置
    vector<Error> errors;             // 错误列表
    vector<Quadruple> quads;          // 四元式列表（中间代码）
    int tempCount;                    // 临时变量计数器
    int labelCount;                   // 标号计数器
    stack<string> loopExitLabels;      // 循环退出标签栈
    vector<map<string, SymbolInfo>> scopeStack; // 作用域栈，实现词法作用域
    map<string, TypeInfo> typeTable;    // 类型表，存储自定义类型信息

public:
    ASTNode* root;                    // 抽象语法树根节点

    // 构造函数 - 初始化语法分析器
    Parser(vector<Token>& t)
        : tokens(t), cur(0), tempCount(0), labelCount(0), root(nullptr) {
        // 初始化基础类型表
        typeTable["integer"] = {false, D_INTEGER};
        typeTable["real"] = {false, D_REAL};
        typeTable["boolean"] = {false, D_BOOLEAN};
    }

    // 主解析方法 - 开始语法分析
    void parse() { root = parseProgram(); }

    // 获取错误列表
    vector<Error> getErrors() { return errors; }
    // 获取四元式列表
    vector<Quadruple> getQuads() { return quads; }

private:
    // 查看当前token - 不移动当前位置，可以指定偏移量
    Token peek(int offset = 0) const {
        int idx = cur + offset;
        if (idx >= static_cast<int>(tokens.size())) return tokens.back();
        return tokens[idx];
    }

    // 获取前一个token
    Token previous() const {
        if (cur == 0) return tokens[0];
        return tokens[cur - 1];
    }

    // 检查是否到达文件结尾
    bool isAtEnd() const { return peek().type == T_EOF; }

    // 前进到下一个token并返回当前token
    Token advance() {
        if (!isAtEnd()) cur++;
        return tokens[cur - 1];
    }

    // 匹配指定类型的token - 如果匹配成功则前进
    bool match(TokenType type) {
        if (peek().type == type) {
            advance();
            return true;
        }
        return false;
    }

    // 期望特定类型的token - 如果不匹配则报错
    void expect(TokenType type, int errCode) {
        if (!match(type)) {
            error(errCode, "Expected token type " + to_string(type), peek().line, true);
        }
    }

    // 错误处理 - 记录错误信息并可选择错误恢复
    void error(int code, const string& msg, int line = -1, bool recover = false) {
        int lineNo = line;
        if (lineNo == -1) lineNo = peek().line;
        errors.push_back({code, lineNo, msg});
        if (recover) {
            // 错误恢复：跳过直到分号、END或ELSE
            while (!isAtEnd() &&
                   peek().type != T_SEMICOLON &&
                   peek().type != T_END &&
                   peek().type != T_ELSE) {
                advance();
            }
        }
    }

    // 生成新的临时变量名 - 用于中间代码生成
    string newTemp() { return "t" + to_string(++tempCount); }
    
    // 生成新的标号 - 用于跳转指令
    string newLabel() { return "L" + to_string(++labelCount); }

    // 生成四元式 - 中间代码生成的基本操作
    void emit(const string& op, const string& arg1, const string& arg2, const string& res) {
        quads.push_back({static_cast<int>(quads.size()) + 1, op, arg1, arg2, res});
    }

    // 进入新作用域 - 实现词法作用域
    void enterScope() { scopeStack.push_back({}); }
    
    // 退出当前作用域
    void exitScope() { if (!scopeStack.empty()) scopeStack.pop_back(); }

    // 声明符号 - 在当前作用域中添加标识符
    void declareSymbol(const string& name, const SymbolInfo& info) {
        if (scopeStack.empty()) enterScope();
        scopeStack.back()[name] = info;
    }

    // 查找符号 - 从内层作用域向外层查找
    SymbolInfo* lookupSymbol(const string& name) {
        for (auto it = scopeStack.rbegin(); it != scopeStack.rend(); ++it) {
            auto found = it->find(name);
            if (found != it->end()) return &found->second;
        }
        return nullptr;
    }

    // 获取符号信息 - 如果未声明则报错
    SymbolInfo getSymbolInfo(const string& name, int line) {
        SymbolInfo* info = lookupSymbol(name);
        if (!info) {
            error(0, "Undeclared identifier '" + name + "'", line);
            return SymbolInfo{};
        }
        return *info;
    }

    bool isRelOp(TokenType t) const {
        return t == T_EQ || t == T_NEQ || t == T_LT || t == T_LE || t == T_GT || t == T_GE;
    }

    bool isAddOp(TokenType t) const {
        return t == T_PLUS || t == T_MINUS || t == T_OR;
    }

    bool isMulOp(TokenType t) const {
        return t == T_MUL || t == T_SLASH || t == T_DIV || t == T_MOD || t == T_AND;
    }

    bool isNumericType(DataType dt) const {
        return dt == D_INTEGER || dt == D_REAL;
    }

    bool isBooleanType(DataType dt) const {
        return dt == D_BOOLEAN;
    }

    DataType combineNumericTypes(DataType left, DataType right) const {
        if (left == D_REAL || right == D_REAL) return D_REAL;
        if (left == D_INTEGER && right == D_INTEGER) return D_INTEGER;
        return D_UNKNOWN;
    }

    bool isAssignable(DataType target, DataType source) const {
        if (target == D_UNKNOWN || source == D_UNKNOWN) return true;
        if (target == source) return true;
        if (target == D_REAL && source == D_INTEGER) return true;
        return false;
    }

    bool validRelational(TokenType op, DataType left, DataType right) const {
        if (left == D_UNKNOWN || right == D_UNKNOWN) return true;
        if ((op == T_EQ || op == T_NEQ) && left == right) return true;
        if (isNumericType(left) && isNumericType(right)) return true;
        return false;
    }

    string dataTypeName(DataType dt) const {
        switch (dt) {
            case D_INTEGER: return "integer";
            case D_REAL: return "real";
            case D_BOOLEAN: return "boolean";
            case D_STRING: return "string";
            case D_VOID: return "void";
            default: return "unknown";
        }
    }

    DataType tokenTypeToDataType(TokenType tt) {
        switch (tt) {
            case T_INTEGER: return D_INTEGER;
            case T_REAL: return D_REAL;
            case T_BOOLEAN: return D_BOOLEAN;
            default: return D_UNKNOWN;
        }
    }

    TypeInfo parseTypeDefinition() {
        TypeInfo info;
        if (match(T_ARRAY)) {
            expect(T_LBRACKET, 29);
            expect(T_NUMBER, 2);
            expect(T_DOTDOT, 29);
            expect(T_NUMBER, 2);
            expect(T_RBRACKET, 22);
            expect(T_OF, 29);
            DataType base = parseBaseType();
            info.isArray = true;
            info.baseType = base;
            return info;
        }
        if (peek().type == T_IDENT) {
            string alias = peek().value;
            auto it = typeTable.find(alias);
            if (it != typeTable.end()) info = it->second;
            else error(0, "Unknown type '" + alias + "'", peek().line);
            advance();
            return info;
        }
        DataType baseType = tokenTypeToDataType(peek().type);
        if (baseType == D_UNKNOWN) error(0, "Unknown type.", peek().line);
        advance();
        info.isArray = false;
        info.baseType = baseType;
        return info;
    }

    DataType parseBaseType() {
        if (peek().type == T_IDENT) {
            string name = peek().value;
            auto it = typeTable.find(name);
            if (it != typeTable.end()) {
                if (it->second.isArray) {
                    error(0, "Array type '" + name + "' cannot be used here.", peek().line);
                    advance();
                    return D_UNKNOWN;
                }
                advance();
                return it->second.baseType;
            }
            error(0, "Unknown type '" + name + "'", peek().line);
            advance();
            return D_UNKNOWN;
        }
        DataType dt = tokenTypeToDataType(peek().type);
        if (dt == D_UNKNOWN) error(0, "Unknown type.", peek().line);
        advance();
        return dt;
    }

    SymbolInfo buildSymbolInfo() {
        SymbolInfo info;
        if (peek().type == T_ARRAY) {
            TypeInfo t = parseTypeDefinition();
            info.isArray = t.isArray;
            info.elementType = t.baseType;
            return info;
        }
        if (peek().type == T_IDENT) {
            string typeName = peek().value;
            auto it = typeTable.find(typeName);
            if (it != typeTable.end()) {
                if (it->second.isArray) {
                    info.isArray = true;
                    info.elementType = it->second.baseType;
                } else {
                    info.valueType = it->second.baseType;
                }
            } else {
                error(0, "Unknown type '" + typeName + "'", peek().line);
            }
            advance();
            return info;
        }
        DataType dt = tokenTypeToDataType(peek().type);
        if (dt == D_UNKNOWN) error(0, "Unknown type.", peek().line);
        else info.valueType = dt;
        advance();
        return info;
    }

    vector<pair<string, SymbolInfo>> parseParameterList() {
        vector<pair<string, SymbolInfo>> params;
        while (peek().type != T_RPAREN && peek().type != T_EOF) {
            if (match(T_SEMICOLON)) continue;
            match(T_VAR);
            vector<string> names;
            do {
                if (peek().type == T_IDENT) {
                    names.push_back(peek().value);
                    advance();
                } else {
                    expect(T_IDENT, 4);
                    break;
                }
            } while (match(T_COMMA));
            expect(T_COLON, 5);
            SymbolInfo info = buildSymbolInfo();
            for (const string& n : names) {
                params.push_back({n, info});
            }
            if (!match(T_SEMICOLON)) break;
        }
        return params;
    }

    Designator parseDesignator(const string& base, const SymbolInfo& sym, int line) {
        Designator d;
        d.name = base;
        d.symbol = sym;
        d.line = line;
        while (match(T_LBRACKET)) {
            ASTNode* idx = parseExp();
            d.indices.push_back(idx->info);
            expect(T_RBRACKET, 22);
        }
        if (!d.indices.empty() && !sym.isArray) {
            error(0, "Identifier '" + base + "' is not an array.", line);
        }
        return d;
    }

    DataType designatorTargetType(const Designator& d) const {
        if (d.indices.empty()) {
            if (d.symbol.isArray) return D_UNKNOWN;
            return d.symbol.valueType;
        }
        return d.symbol.elementType;
    }

    string loadDesignatorValue(const Designator& d) {
        if (d.indices.empty()) {
            if (d.symbol.isArray) {
                error(0, "Array '" + d.name + "' requires an index.", d.line);
            }
            return d.name;
        }
        string current = d.name;
        for (const string& idx : d.indices) {
            string temp = newTemp();
            emit("array_load", current, idx, temp);
            current = temp;
        }
        return current;
    }

    void storeDesignatorValue(const Designator& d, const string& value) {
        if (d.indices.empty()) {
            if (d.symbol.isArray) {
                error(0, "Cannot assign to entire array '" + d.name + "'.", d.line);
                return;
            }
            emit(":=", value, "", d.name);
            return;
        }
        string current = d.name;
        for (size_t i = 0; i + 1 < d.indices.size(); ++i) {
            string temp = newTemp();
            emit("array_load", current, d.indices[i], temp);
            current = temp;
        }
        emit("array_store", current, d.indices.back(), value);
    }

    ASTNode* parseProgram() {
        ASTNode* node = new ASTNode("Program");
        match(T_PROGRAM);
        enterScope();
        node->add(parseBlock(false));
        exitScope();
        expect(T_PERIOD, 9);
        if (peek().type != T_EOF) {
            error(47, "Unexpected tokens after program end.");
        }
        return node;
    }

    ASTNode* parseBlock(bool createScope = true) {
        if (createScope) enterScope();
        ASTNode* node = new ASTNode("Block");
        if (peek().type == T_CONST) {
            advance();
            do {
                expect(T_IDENT, 4);
                expect(T_EQ, 1);
                if (peek().type == T_NUMBER || peek().type == T_STRING || peek().type == T_TRUE || peek().type == T_FALSE) {
                    advance();
                } else {
                    error(2, "Invalid const value.", peek().line);
                }
                expect(T_SEMICOLON, 5);
            } while (peek().type == T_IDENT);
            node->add(new ASTNode("ConstDecls"));
        }

        if (peek().type == T_TYPE) {
            advance();
            while (peek().type == T_IDENT) {
                string name = peek().value;
                advance();
                expect(T_EQ, 3);
                TypeInfo info = parseTypeDefinition();
                typeTable[name] = info;
                expect(T_SEMICOLON, 5);
            }
            node->add(new ASTNode("TypeDecls"));
        }

        if (peek().type == T_VAR) {
            advance();
            while (peek().type == T_IDENT) {
                vector<string> names;
                do {
                    names.push_back(peek().value);
                    advance();
                } while (match(T_COMMA));
                expect(T_COLON, 5);
                SymbolInfo info = buildSymbolInfo();
                expect(T_SEMICOLON, 5);
                for (const string& n : names) declareSymbol(n, info);
            }
            node->add(new ASTNode("VarDecls"));
        }

        while (peek().type == T_PROCEDURE || peek().type == T_FUNCTION) {
            bool isFunc = (peek().type == T_FUNCTION);
            ASTNode* sub = new ASTNode(isFunc ? "FuncDecl" : "ProcDecl");
            advance();
            string name;
            if (peek().type == T_IDENT) {
                name = peek().value;
                advance();
            } else {
                expect(T_IDENT, 4);
            }
            vector<pair<string, SymbolInfo>> params;
            if (match(T_LPAREN)) {
                params = parseParameterList();
                expect(T_RPAREN, 22);
            }
            DataType retType = D_VOID;
            if (isFunc) {
                expect(T_COLON, 5);
                retType = parseBaseType();
                SymbolInfo funcInfo;
                funcInfo.valueType = retType;
                declareSymbol(name, funcInfo);
            } else {
                SymbolInfo procInfo;
                procInfo.valueType = D_VOID;
                declareSymbol(name, procInfo);
            }
            expect(T_SEMICOLON, 5);
            enterScope();
            for (const auto& param : params) {
                declareSymbol(param.first, param.second);
            }
            sub->add(parseBlock(false));
            exitScope();
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
        expect(T_END, 17);
        if (createScope) exitScope();
        return node;
    }

    ASTNode* parseStmt() {
        ASTNode* node = new ASTNode("Stmt");
        if (peek().type == T_IDENT) {
            Token identTok = advance();
            string name = identTok.value;
            SymbolInfo sym = getSymbolInfo(name, identTok.line);
            Designator designator = parseDesignator(name, sym, identTok.line);
            expect(T_ASSIGN, 13);
            ASTNode* exprNode = parseExp();
            DataType targetType = designatorTargetType(designator);
            if (!isAssignable(targetType, exprNode->dataType)) {
                error(0, "Type mismatch: Cannot assign " + dataTypeName(exprNode->dataType) +
                             " to '" + name + "' (" + dataTypeName(targetType) + ")", identTok.line);
            }
            storeDesignatorValue(designator, exprNode->info);
            node->info = "Assign";
        } else if (match(T_CALL)) {
            node->info = "Call";
            string name;
            if (peek().type == T_IDENT) {
                name = peek().value;
                advance();
            } else {
                expect(T_IDENT, 14);
            }
            int argCount = 0;
            if (match(T_LPAREN)) {
                if (peek().type != T_RPAREN) {
                    do {
                        ASTNode* arg = parseExp();
                        emit("param", "", "", arg->info);
                        argCount++;
                    } while (match(T_COMMA));
                }
                expect(T_RPAREN, 22);
            }
            emit("call", name, to_string(argCount), "");
        } else if (match(T_IF)) {
            node->info = "If";
            ASTNode* cond = parseExp();
            if (!isBooleanType(cond->dataType) && cond->dataType != D_UNKNOWN) {
                error(0, "IF condition must be boolean.", previous().line);
            }
            string condPlace = cond->info;
            string L_true = newLabel();
            string L_false = newLabel();
            emit("jnz", condPlace, "", L_true);
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
        } else if (match(T_WHILE)) {
            node->info = "While";
            string startLabel = newLabel();
            string endLabel = newLabel();
            loopExitLabels.push(endLabel);
            emit("label", "", "", startLabel);
            ASTNode* cond = parseExp();
            if (!isBooleanType(cond->dataType) && cond->dataType != D_UNKNOWN) {
                error(0, "WHILE condition must be boolean.", previous().line);
            }
            emit("jz", cond->info, "", endLabel);
            expect(T_DO, 26);
            node->add(parseStmt());
            emit("j", "", "", startLabel);
            emit("label", "", "", endLabel);
            loopExitLabels.pop();
        } else if (match(T_BEGIN)) {
            node->info = "Compound";
            if (peek().type != T_END) {
                node->add(parseStmt());
                while (match(T_SEMICOLON)) {
                    if (peek().type == T_END) break;
                    node->add(parseStmt());
                }
            }
            expect(T_END, 17);
        } else if (match(T_WRITE)) {
            node->info = "Write";
            expect(T_LPAREN, 34);
            do {
                ASTNode* expr = parseExp();
                emit("write", "", "", expr->info);
            } while (match(T_COMMA));
            expect(T_RPAREN, 34);
        } else if (match(T_EXIT)) {
            node->info = "Exit";
            if (loopExitLabels.empty()) {
                error(0, "EXIT statement not within a loop.", previous().line);
            } else {
                emit("j", "", "", loopExitLabels.top());
            }
        } else if (match(T_READ)) {
            node->info = "Read";
            expect(T_LPAREN, 34);
            do {
                if (peek().type == T_IDENT) {
                    Token tok = advance();
                    SymbolInfo sym = getSymbolInfo(tok.value, tok.line);
                    Designator dest = parseDesignator(tok.value, sym, tok.line);
                    emit("read", "", "", dest.name);
                } else {
                    expect(T_IDENT, 4);
                }
            } while (match(T_COMMA));
            expect(T_RPAREN, 34);
        } else {
            advance();
        }
        return node;
    }

    ASTNode* parseExp() {
        ASTNode* left = parseSimpleExp();
        if (isRelOp(peek().type)) {
            TokenType opType = peek().type;
            string op = peek().value;
            advance();
            ASTNode* right = parseSimpleExp();
            if (!validRelational(opType, left->dataType, right->dataType)) {
                error(0, "Relational operation requires compatible types (" +
                             dataTypeName(left->dataType) + " vs " +
                             dataTypeName(right->dataType) + ")",
                      previous().line);
            }
            ASTNode* node = new ASTNode("RelOp", op);
            node->add(left);
            node->add(right);
            string temp = newTemp();
            emit(op, left->info, right->info, temp);
            node->info = temp;
            node->dataType = D_BOOLEAN;
            return node;
        }
        return left;
    }

    ASTNode* parseSimpleExp() {
        ASTNode* node = parseTerm();
        bool unaryMinus = false;
        if (node->type == "UnaryPendingMinus") {
            unaryMinus = true;
        }
        if (unaryMinus) {
            if (!isNumericType(node->dataType) && node->dataType != D_UNKNOWN) {
                error(0, "Unary minus requires numeric operand.", previous().line);
            }
            string temp = newTemp();
            emit("uminus", node->info, "", temp);
            node->info = temp;
        }
        while (isAddOp(peek().type)) {
            TokenType opType = peek().type;
            string op = peek().value;
            advance();
            ASTNode* right = parseTerm();
            ASTNode* opNode = new ASTNode("BinOp", op);
            opNode->add(node);
            opNode->add(right);
            if (opType == T_OR) {
                if ((!isBooleanType(node->dataType) && node->dataType != D_UNKNOWN) ||
                    (!isBooleanType(right->dataType) && right->dataType != D_UNKNOWN)) {
                    error(0, "OR requires boolean operands.", previous().line);
                }
                opNode->dataType = D_BOOLEAN;
                string temp = newTemp();
                emit("or", node->info, right->info, temp);
                opNode->info = temp;
            } else {
                if ((!isNumericType(node->dataType) && node->dataType != D_UNKNOWN) ||
                    (!isNumericType(right->dataType) && right->dataType != D_UNKNOWN)) {
                    error(0, "Arithmetic operation requires numeric operands.", previous().line);
                }
                opNode->dataType = combineNumericTypes(node->dataType, right->dataType);
                string temp = newTemp();
                emit(op, node->info, right->info, temp);
                opNode->info = temp;
            }
            node = opNode;
        }
        return node;
    }

    ASTNode* parseTerm() {
        ASTNode* node = parseFactor();
        while (isMulOp(peek().type)) {
            TokenType opType = peek().type;
            string op = peek().value;
            advance();
            ASTNode* right = parseFactor();
            ASTNode* opNode = new ASTNode("BinOp", op);
            opNode->add(node);
            opNode->add(right);
            if (opType == T_AND) {
                if ((!isBooleanType(node->dataType) && node->dataType != D_UNKNOWN) ||
                    (!isBooleanType(right->dataType) && right->dataType != D_UNKNOWN)) {
                    error(0, "AND requires boolean operands.", previous().line);
                }
                opNode->dataType = D_BOOLEAN;
                string temp = newTemp();
                emit("and", node->info, right->info, temp);
                opNode->info = temp;
            } else if (opType == T_DIV || opType == T_MOD) {
                if ((node->dataType != D_INTEGER && node->dataType != D_UNKNOWN) ||
                    (right->dataType != D_INTEGER && right->dataType != D_UNKNOWN)) {
                    error(0, "DIV/MOD require integer operands.", previous().line);
                }
                opNode->dataType = D_INTEGER;
                string temp = newTemp();
                emit(op, node->info, right->info, temp);
                opNode->info = temp;
            } else {
                if ((!isNumericType(node->dataType) && node->dataType != D_UNKNOWN) ||
                    (!isNumericType(right->dataType) && right->dataType != D_UNKNOWN)) {
                    error(0, "Arithmetic operation requires numeric operands.", previous().line);
                }
                opNode->dataType = combineNumericTypes(node->dataType, right->dataType);
                string temp = newTemp();
                emit(op, node->info, right->info, temp);
                opNode->info = temp;
            }
            node = opNode;
        }
        return node;
    }

    ASTNode* parseFactor() {
        ASTNode* node = new ASTNode("Factor");
        if (peek().type == T_PLUS || peek().type == T_MINUS) {
            bool isMinus = match(T_MINUS);
            if (!isMinus) advance();
            ASTNode* factor = parseFactor();
            if (isMinus) {
                if (!isNumericType(factor->dataType) && factor->dataType != D_UNKNOWN) {
                    error(0, "Unary minus requires numeric operand.", previous().line);
                }
                string temp = newTemp();
                emit("uminus", factor->info, "", temp);
                factor->info = temp;
            }
            return factor;
        }
        switch (peek().type) {
            case T_IDENT: {
                Token identTok = advance();
                string name = identTok.value;
                if (peek().type == T_LPAREN) {
                    vector<ASTNode*> args;
                    advance();
                    if (peek().type != T_RPAREN) {
                        do {
                            args.push_back(parseExp());
                        } while (match(T_COMMA));
                    }
                    expect(T_RPAREN, 22);
                    for (ASTNode* arg : args) emit("param", "", "", arg->info);
                    string temp = newTemp();
                    emit("call", name, to_string(args.size()), temp);
                    node->info = temp;
                    SymbolInfo* info = lookupSymbol(name);
                    node->dataType = (info) ? info->valueType : D_UNKNOWN;
                } else {
                    SymbolInfo sym = getSymbolInfo(name, identTok.line);
                    Designator designator = parseDesignator(name, sym, identTok.line);
                    node->info = loadDesignatorValue(designator);
                    node->dataType = designator.indices.empty() ?
                        (sym.isArray ? D_UNKNOWN : sym.valueType) : sym.elementType;
                }
                break;
            }
            case T_NUMBER: {
                string literal = peek().value;
                node->info = literal;
                node->dataType = (literal.find('.') != string::npos) ? D_REAL : D_INTEGER;
                advance();
                break;
            }
            case T_STRING: {
                node->info = "'" + peek().value + "'";
                node->dataType = D_STRING;
                advance();
                break;
            }
            case T_LPAREN: {
                advance();
                node = parseExp();
                expect(T_RPAREN, 22);
                break;
            }
            case T_NOT: {
                advance();
                ASTNode* inner = parseFactor();
                if ((!isBooleanType(inner->dataType) && inner->dataType != D_UNKNOWN)) {
                    error(0, "NOT requires boolean operand.", previous().line);
                }
                string temp = newTemp();
                emit("not", inner->info, "", temp);
                node->info = temp;
                node->dataType = D_BOOLEAN;
                break;
            }
            case T_ODD: {
                advance();
                expect(T_LPAREN, 34);
                ASTNode* expr = parseExp();
                expect(T_RPAREN, 34);
                string temp = newTemp();
                emit("odd", expr->info, "", temp);
                node->info = temp;
                node->dataType = D_BOOLEAN;
                break;
            }
            case T_TRUE: {
                node->info = "1";
                node->dataType = D_BOOLEAN;
                advance();
                break;
            }
            case T_FALSE: {
                node->info = "0";
                node->dataType = D_BOOLEAN;
                advance();
                break;
            }
            default:
                error(24, "Expression cannot start with token: " + peek().value, peek().line);
                advance();
                node->info = newTemp();
                break;
        }
        return node;
    }
};

// JSON字符串转义函数 - 处理JSON中的特殊字符
// 确保输出到JSON文件中的字符串格式正确
string escapeJsonString(const string& input) {
    stringstream ss;
    for (char c : input) {
        switch (c) {
            case '"': ss << "\\\""; break;   // 双引号转义
            case '\\': ss << "\\\\"; break;   // 反斜杠转义
            case '\b': ss << "\\b"; break;    // 退格符转义
            case '\f': ss << "\\f"; break;    // 换页符转义
            case '\n': ss << "\\n"; break;    // 换行符转义
            case '\r': ss << "\\r"; break;    // 回车符转义
            case '\t': ss << "\\t"; break;    // 制表符转义
            default: ss << c; break;          // 其他字符直接输出
        }
    }
    return ss.str();
}

// 编译结果JSON格式化函数 - 将编译结果转换为JSON格式输出
// 包含文件名、源代码、token序列、错误信息、四元式和抽象语法树
string getCompilationResultAsJson(const string& filename,
                                  const string& source,
                                  const vector<Token>& tokens,
                                  ASTNode* ast,
                                  const vector<Quadruple>& quads,
                                  const vector<Error>& errs) {
    stringstream ss;
    ss << "{\n";
    ss << "  \"filename\": \"" << filename << "\",\n";
    ss << "  \"source\": \"" << escapeJsonString(source) << "\",\n";
    
    // 输出token序列
    ss << "  \"tokens\": [\n";
    for (size_t i = 0; i < tokens.size(); ++i) {
        ss << "    {\"id\": " << tokens[i].id << ", \"line\": " << tokens[i].line
           << ", \"type\": " << tokens[i].type << ", \"value\": \"" << tokens[i].value << "\"}"
           << (i + 1 < tokens.size() ? "," : "") << "\n";
    }
    ss << "  ],\n";
    
    // 输出错误信息
    ss << "  \"errors\": [\n";
    for (size_t i = 0; i < errs.size(); ++i) {
        ss << "    {\"code\": " << errs[i].code << ", \"line\": " << errs[i].line
           << ", \"msg\": \"" << escapeJsonString(errs[i].msg) << "\"}"
           << (i + 1 < errs.size() ? "," : "") << "\n";
    }
    ss << "  ],\n";
    
    // 输出四元式（中间代码）
    ss << "  \"tac\": [\n";
    for (size_t i = 0; i < quads.size(); ++i) {
        ss << "    {\"id\": " << quads[i].id << ", \"op\": \"" << quads[i].op
           << "\", \"arg1\": \"" << quads[i].arg1 << "\", \"arg2\": \"" << quads[i].arg2
           << "\", \"result\": \"" << quads[i].result << "\"}"
           << (i + 1 < quads.size() ? "," : "") << "\n";
    }
    ss << "  ],\n";
    
    // 输出抽象语法树
    ss << "  \"ast\": ";
    // 使用lambda表达式递归打印AST节点
    function<void(ASTNode*)> printNode = [&](ASTNode* node) {
        if (!node) { ss << "null"; return; }
        ss << "{\"id\": " << node->id << ", \"type\": \"" << node->type
           << "\", \"info\": \"" << escapeJsonString(node->info) << "\", \"children\": [";
        for (size_t i = 0; i < node->children.size(); ++i) {
            printNode(node->children[i]);
            if (i + 1 < node->children.size()) ss << ",";
        }
        ss << "]}";
    };
    printNode(ast);
    ss << "\n}";
    return ss.str();
}

// 主函数 - 编译器入口点
// 负责处理命令行参数、文件I/O和编译流程控制
int main(int argc, char* argv[]) {
    vector<string> filesToCompile;
    
    // 处理命令行参数：如果没有指定文件，则编译test0.pl到test9.pl
    if (argc == 1) {
        for (int i = 0; i <= 9; i++) filesToCompile.push_back("test" + to_string(i) + ".pl");
    } else {
        // 如果指定了文件，则编译指定的文件
        for (int i = 1; i < argc; ++i) filesToCompile.push_back(argv[i]);
    }

    // 打开输出文件output.json
    ofstream outFile("output.json");
    if (!outFile.is_open()) {
        cerr << "Error: Cannot create output.json" << endl;
        return 1;
    }

    outFile << "{\n";  // 开始JSON对象
    
    // 逐个编译文件
    for (size_t i = 0; i < filesToCompile.size(); ++i) {
        const string& filename = filesToCompile[i];
        ifstream file(filename);
        if (!file.is_open()) {
            cerr << "Error: Cannot open " << filename << endl;
            continue;
        }
        
        cout << "Compiling " << filename << "..." << endl;
        
        // 读取源代码
        stringstream buffer;
        buffer << file.rdbuf();
        string source = buffer.str();
        
        // 执行编译流程
        Lexer lexer(source);        // 词法分析
        lexer.scan();             // 生成token序列
        Parser parser(lexer.tokens); // 语法分析
        parser.parse();            // 构建AST并生成中间代码
        
        // 将编译结果转换为JSON格式并写入文件
        string resultJson = getCompilationResultAsJson(filename, source, lexer.tokens, parser.root, parser.getQuads(), parser.getErrors());
        outFile << "  \"" << filename << "\": " << resultJson;
        if (i + 1 < filesToCompile.size()) outFile << ",\n";
    }
    
    outFile << "\n}\n";  // 结束JSON对象
    outFile.close();
    
    cout << "Compilation finished. All results saved to output.json" << endl;
    return 0;
}
