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

enum TokenType {
    T_NULL, T_IDENT, T_NUMBER, T_STRING, T_PROGRAM,
    T_CONST, T_VAR, T_PROCEDURE, T_FUNCTION, T_BEGIN, T_END, T_IF, T_THEN,
    T_ELSE, T_WHILE, T_DO, T_CALL, T_READ, T_WRITE, T_EXIT, T_ODD,
    T_TYPE, T_ARRAY, T_OF, T_OR, T_AND, T_NOT, T_DIV, T_MOD,
    T_TRUE, T_FALSE, T_INTEGER, T_REAL, T_BOOLEAN,
    T_PLUS, T_MINUS, T_MUL, T_SLASH, T_EQ, T_NEQ, T_LT, T_LE, T_GT, T_GE,
    T_LPAREN, T_RPAREN, T_LBRACKET, T_RBRACKET, T_COMMA, T_SEMICOLON,
    T_PERIOD, T_ASSIGN, T_COLON, T_DOTDOT, T_EOF
};

struct Token {
    int id = 0;
    int line = 0;
    TokenType type = T_NULL;
    string value;
    int intVal = 0;
    double realVal = 0.0;
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

class Lexer {
private:
    string src;
    size_t pos;
    int line;
    int tokenCount;
    map<string, TokenType> keywords;

public:
    vector<Token> tokens;

    Lexer(string source) : src(move(source)), pos(0), line(1), tokenCount(0) {
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
            if (isspace(static_cast<unsigned char>(ch))) {
                if (ch == '\n') line++;
                pos++;
            } else if (ch == '/' && pos + 1 < src.length() && src[pos + 1] == '*') {
                pos += 2;
                while (pos + 1 < src.length() && !(src[pos] == '*' && src[pos + 1] == '/')) {
                    if (src[pos] == '\n') line++;
                    pos++;
                }
                if (pos + 1 < src.length()) pos += 2;
            } else if (isalpha(static_cast<unsigned char>(ch))) {
                string buf;
                while (pos < src.length() && isalnum(static_cast<unsigned char>(src[pos]))) {
                    buf += src[pos++];
                }
                TokenType type = T_IDENT;
                string lowerBuf = buf;
                transform(lowerBuf.begin(), lowerBuf.end(), lowerBuf.begin(), ::tolower);
                if (keywords.count(lowerBuf)) {
                    type = keywords[lowerBuf];
                    buf = lowerBuf;
                }
                addToken(type, buf);
            } else if (isdigit(static_cast<unsigned char>(ch))) {
                string buf;
                bool isReal = false;
                while (pos < src.length() && isdigit(static_cast<unsigned char>(src[pos]))) {
                    buf += src[pos++];
                }
                if (pos < src.length() && src[pos] == '.') {
                    if (pos + 1 < src.length() && src[pos + 1] == '.') {
                        // part of ..
                    } else {
                        isReal = true;
                        buf += src[pos++];
                        while (pos < src.length() && isdigit(static_cast<unsigned char>(src[pos]))) {
                            buf += src[pos++];
                        }
                    }
                }
                addToken(T_NUMBER, buf);
                tokens.back().realVal = stod(buf);
                if (!isReal) tokens.back().intVal = stoi(buf);
            } else if (ch == '\'') {
                string buf;
                pos++;
                while (pos < src.length() && src[pos] != '\'') {
                    if (src[pos] == '\n') line++;
                    buf += src[pos++];
                }
                if (pos < src.length()) pos++;
                addToken(T_STRING, buf);
            } else {
                pos++;
                switch (ch) {
                    case ':':
                        if (pos < src.length() && src[pos] == '=') {
                            pos++;
                            addToken(T_ASSIGN, ":=");
                        } else addToken(T_COLON, ":");
                        break;
                    case '<':
                        if (pos < src.length() && src[pos] == '=') {
                            pos++;
                            addToken(T_LE, "<=");
                        } else if (pos < src.length() && src[pos] == '>') {
                            pos++;
                            addToken(T_NEQ, "<>");
                        } else addToken(T_LT, "<");
                        break;
                    case '>':
                        if (pos < src.length() && src[pos] == '=') {
                            pos++;
                            addToken(T_GE, ">=");
                        } else addToken(T_GT, ">");
                        break;
                    case '.':
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
                    default: break;
                }
            }
        }
        addToken(T_EOF, "EOF");
    }

    void addToken(TokenType type, const string& val) {
        Token t;
        t.id = ++tokenCount;
        t.line = line;
        t.type = type;
        t.value = val;
        tokens.push_back(t);
    }
};

enum DataType { D_UNKNOWN, D_INTEGER, D_REAL, D_BOOLEAN, D_STRING, D_VOID };

struct SymbolInfo {
    DataType valueType = D_UNKNOWN;
    bool isArray = false;
    DataType elementType = D_UNKNOWN;
};

struct TypeInfo {
    bool isArray = false;
    DataType baseType = D_UNKNOWN;
};

struct ASTNode {
    int id;
    static int global_id;
    string type;
    string info;
    DataType dataType = D_UNKNOWN;
    vector<ASTNode*> children;
    ASTNode(string t, string i = "") : type(move(t)), info(move(i)) { id = ++global_id; }
    void add(ASTNode* node) { if (node) children.push_back(node); }
};
int ASTNode::global_id = 0;

class Parser {
    struct Designator {
        string name;
        vector<string> indices;
        SymbolInfo symbol;
        int line;
    };

    vector<Token>& tokens;
    int cur;
    vector<Error> errors;
    vector<Quadruple> quads;
    int tempCount;
    int labelCount;
    stack<string> loopExitLabels;
    vector<map<string, SymbolInfo>> scopeStack;
    map<string, TypeInfo> typeTable;

public:
    ASTNode* root;

    Parser(vector<Token>& t)
        : tokens(t), cur(0), tempCount(0), labelCount(0), root(nullptr) {
        typeTable["integer"] = {false, D_INTEGER};
        typeTable["real"] = {false, D_REAL};
        typeTable["boolean"] = {false, D_BOOLEAN};
    }

    void parse() { root = parseProgram(); }

    vector<Error> getErrors() { return errors; }
    vector<Quadruple> getQuads() { return quads; }

private:
    Token peek(int offset = 0) const {
        int idx = cur + offset;
        if (idx >= static_cast<int>(tokens.size())) return tokens.back();
        return tokens[idx];
    }

    Token previous() const {
        if (cur == 0) return tokens[0];
        return tokens[cur - 1];
    }

    bool isAtEnd() const { return peek().type == T_EOF; }

    Token advance() {
        if (!isAtEnd()) cur++;
        return tokens[cur - 1];
    }

    bool match(TokenType type) {
        if (peek().type == type) {
            advance();
            return true;
        }
        return false;
    }

    void expect(TokenType type, int errCode) {
        if (!match(type)) {
            error(errCode, "Expected token type " + to_string(type), peek().line, true);
        }
    }

    void error(int code, const string& msg, int line = -1, bool recover = false) {
        int lineNo = line;
        if (lineNo == -1) lineNo = peek().line;
        errors.push_back({code, lineNo, msg});
        if (recover) {
            while (!isAtEnd() &&
                   peek().type != T_SEMICOLON &&
                   peek().type != T_END &&
                   peek().type != T_ELSE) {
                advance();
            }
        }
    }

    string newTemp() { return "t" + to_string(++tempCount); }
    string newLabel() { return "L" + to_string(++labelCount); }

    void emit(const string& op, const string& arg1, const string& arg2, const string& res) {
        quads.push_back({static_cast<int>(quads.size()) + 1, op, arg1, arg2, res});
    }

    void enterScope() { scopeStack.push_back({}); }
    void exitScope() { if (!scopeStack.empty()) scopeStack.pop_back(); }

    void declareSymbol(const string& name, const SymbolInfo& info) {
        if (scopeStack.empty()) enterScope();
        scopeStack.back()[name] = info;
    }

    SymbolInfo* lookupSymbol(const string& name) {
        for (auto it = scopeStack.rbegin(); it != scopeStack.rend(); ++it) {
            auto found = it->find(name);
            if (found != it->end()) return &found->second;
        }
        return nullptr;
    }

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

string escapeJsonString(const string& input) {
    stringstream ss;
    for (char c : input) {
        switch (c) {
            case '"': ss << "\\\""; break;
            case '\\': ss << "\\\\"; break;
            case '\b': ss << "\\b"; break;
            case '\f': ss << "\\f"; break;
            case '\n': ss << "\\n"; break;
            case '\r': ss << "\\r"; break;
            case '\t': ss << "\\t"; break;
            default: ss << c; break;
        }
    }
    return ss.str();
}

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
    ss << "  \"tokens\": [\n";
    for (size_t i = 0; i < tokens.size(); ++i) {
        ss << "    {\"id\": " << tokens[i].id << ", \"line\": " << tokens[i].line
           << ", \"type\": " << tokens[i].type << ", \"value\": \"" << tokens[i].value << "\"}"
           << (i + 1 < tokens.size() ? "," : "") << "\n";
    }
    ss << "  ],\n";
    ss << "  \"errors\": [\n";
    for (size_t i = 0; i < errs.size(); ++i) {
        ss << "    {\"code\": " << errs[i].code << ", \"line\": " << errs[i].line
           << ", \"msg\": \"" << escapeJsonString(errs[i].msg) << "\"}"
           << (i + 1 < errs.size() ? "," : "") << "\n";
    }
    ss << "  ],\n";
    ss << "  \"tac\": [\n";
    for (size_t i = 0; i < quads.size(); ++i) {
        ss << "    {\"id\": " << quads[i].id << ", \"op\": \"" << quads[i].op
           << "\", \"arg1\": \"" << quads[i].arg1 << "\", \"arg2\": \"" << quads[i].arg2
           << "\", \"result\": \"" << quads[i].result << "\"}"
           << (i + 1 < quads.size() ? "," : "") << "\n";
    }
    ss << "  ],\n";
    ss << "  \"ast\": ";
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

int main(int argc, char* argv[]) {
    vector<string> filesToCompile;
    if (argc == 1) {
        for (int i = 0; i <= 9; i++) filesToCompile.push_back("test" + to_string(i) + ".pl");
    } else {
        for (int i = 1; i < argc; ++i) filesToCompile.push_back(argv[i]);
    }

    ofstream outFile("output.json");
    if (!outFile.is_open()) {
        cerr << "Error: Cannot create output.json" << endl;
        return 1;
    }

    outFile << "{\n";
    for (size_t i = 0; i < filesToCompile.size(); ++i) {
        const string& filename = filesToCompile[i];
        ifstream file(filename);
        if (!file.is_open()) {
            cerr << "Error: Cannot open " << filename << endl;
            continue;
        }
        cout << "Compiling " << filename << "..." << endl;
        stringstream buffer;
        buffer << file.rdbuf();
        string source = buffer.str();
        Lexer lexer(source);
        lexer.scan();
        Parser parser(lexer.tokens);
        parser.parse();
        string resultJson = getCompilationResultAsJson(filename, source, lexer.tokens, parser.root, parser.getQuads(), parser.getErrors());
        outFile << "  \"" << filename << "\": " << resultJson;
        if (i + 1 < filesToCompile.size()) outFile << ",\n";
    }
    outFile << "\n}\n";
    outFile.close();
    cout << "Compilation finished. All results saved to output.json" << endl;
    return 0;
}
