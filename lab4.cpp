#include <iostream>
#include <vector>
#include <map>
enum TokenType {
    T_ADD,  // +
    T_MUL,  // *
    T_LB,   // (
    T_RB,   // )
    T_ID,   // id
    T_EOF   // End of File
};

std::map<TokenType, std::string> tokenToString = {
    {T_ADD, "+"},
    {T_MUL, "*"},
    {T_LB, "("},
    {T_RB, ")"},
    {T_ID, "id"},
    {T_EOF, "#"}
};


std::vector<TokenType> input1 = {T_ID, T_MUL, T_LB, T_ID, T_ADD, T_ID, T_RB, T_EOF};
std::vector<TokenType> input2 = {T_ID, T_ADD, T_MUL, T_ID, T_RB, T_EOF};
std::vector<TokenType> input3 = {T_LB, T_ID, T_ADD, T_ID, T_RB, T_MUL, T_ID, T_EOF};
std::vector<std::vector<TokenType>> inputs = {input1, input2, input3};

int currentPos = 0;
TokenType lookahead;
std::vector<TokenType>* currentInput = nullptr;

void parse_E();
void parse_Eprime();
void parse_T();
void parse_Tprime();
void parse_F();


// 获取下一个 Token
void nextToken() {
    if (currentInput && currentPos < currentInput->size()) {
        lookahead = (*currentInput)[currentPos++];
    } else {
        lookahead = T_EOF;
    }
}


// 匹配并消耗终结符
void match(TokenType expected) {
    if (lookahead == expected) {
        nextToken();
    } else {
        std::cout << "Unexpected token" << std::endl;
    }
}

// E -> T E'
void parse_E() {
    parse_T();
    parse_Eprime();
}

// E' -> + T E' | epsilon
void parse_Eprime() {
    if (lookahead == T_ADD) {
        match(T_ADD);
        parse_T();
        parse_Eprime();
    }
    else if (lookahead == T_RB || lookahead == T_EOF) {
        return; // do nothing (epsilon)
    }
    else {
        std::cout << "Invalid token in E'" << std::endl;
    }
}

// T -> F T'
void parse_T() {
    parse_F();
    parse_Tprime();
}

// T' -> * F T' | epsilon
void parse_Tprime() {
    if (lookahead == T_MUL) {
        match(T_MUL);
        parse_F();
        parse_Tprime();
    }
    else if (lookahead == T_ADD || lookahead == T_RB || lookahead == T_EOF) {
        return; // do nothing (epsilon)
    }
    else {
        std::cout << "Invalid token in T'" << std::endl;
    }
}

// F -> ( E ) | id
void parse_F() {
    if (lookahead == T_LB) {
        match(T_LB);
        parse_E();
        match(T_RB);
    }
    else if (lookahead == T_ID) {
        match(T_ID);
    }
    else {
        std::cout << "Expected '(' or 'id'" << std::endl;
    }
}

int main() {
    for(auto& inp : inputs) {
        std::cout << "Input: ";
        for(auto token : inp) {
            std::cout << tokenToString[token] << " ";
        }
        std::cout << std::endl;
        currentInput = &inp;
        currentPos = 0;
        nextToken();
        parse_E();
        if (lookahead == T_EOF) {
            std::cout << "Success! The input string matches the grammar." << std::endl;
        } else {
            std::cout << "Error: Extra characters at the end." << std::endl;
        }
    }
    return 0;
}
