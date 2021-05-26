#include <QCoreApplication>
#include <QRegularExpression>
#include <QStack>
#include <QVector>
#include <iomanip>
#include <iostream>
#include <list>
#include <map>
#include <optional>
#include <regex>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>
#define OK(_inKind) (match_it->type == TokenKind::_inKind)
#define MATCH(kind)                                                       \
    if (match_it->type == TokenKind::kind) {                              \
        match_it++;                                                       \
    } else {                                                              \
        std::cerr << "Unexpected token: wanted " << TokenKind::kind       \
                  << " but got " << match_it->type << std::endl;          \
        throw "err";                                                      \
    }

enum class TokenKind
{
    FOR_KEY,
    WHILE_KEY,
    IF_KEY,
    ELSE_KEY,
    INC,
    DEC,
    LPar,   // (
    RPar,   // )
    LBrace, // [
    RBRace, // ]
    LCurly, // {
    RCurly, // }
    SMCLN,  // ;
    DDOT,   // :
    COMM,   // ,
    OP,
    Ident,
    ASSIGN_OP,
    NUMB,
    SPACE,
    NEW_KEY,
    FLOAT
};


enum class AstKind
{
    lang,
    stmt,
    while_stmt,
    if_stmt,
    assign_expr,
    expr,
    list,       // (
    else_stmt,  // )
    if_body,    // [
    if_head,    // ]
    else_body,  // {
    while_body, // }
    while_head, // ;
    value,      // :
    else_head,
    Ident,
    NUMB,
    LPar,
    OP,
    EQUAL,
    new_expr,
    call,
    FLOAT
};

enum class ValueKind
{
    Int,
    Float,
    HashTable,
    LinkedList,
    String,
    Bool,
    Ident
};

struct Value;

struct List {
    Value* data;
    List*  prev;
    List*  next;

    List() {
        data = nullptr;
        prev = nullptr;
        next = nullptr;
    }

    void insert(Value* newdata) {
        auto newNode  = new List();
        newNode->data = newdata;
        newNode->next = nullptr;

        auto ptr = this;
        while (ptr->next != nullptr) {
            ptr = ptr->next;
        }

        ptr->next     = newNode;
        newNode->prev = ptr;
    }

    Value* get(int newdata) {
        auto ptr   = this;
        int  index = 0;
        while (ptr != nullptr) {
            if (index == newdata) {
                return ptr->data;
            } else {
                ptr = ptr->next;
                ++index;
            }
        }
    }
};

bool operator==(const Value& a, const Value& b);

class Table
{
  public:
    struct Pair {
        Value* key;
        Value* value;
        Pair() = default;
    };

    struct Bucket {
        QVector<Pair> entries;
        Bucket() = default;
    };

    QVector<Bucket> bucket;

    Table() {
        elements_amount = 0;
        bucket          = QVector<Bucket>(4);
    };

    int elements_amount;

    int Hash(Value* key);

    void insert(Value* key, Value* value, bool recount = true);

    Value* get_table(Value* key);

    void resize();
};


struct Value {
  private:
    ValueKind kind;
    int       intVal;
    float     floatVal;
    QString   strVal;
    bool      boolVal;
    List*     listVal;
    Table*    tableVal;

  public:
    Value() = default;

    int hash() {
        return this->intVal;
    }

    friend bool operator==(const Value& a, const Value& b);

    int getIntVal() const {
        assert(kind == ValueKind::Int);
        return intVal;
    }


    QString getStrVal() const {
        assert(kind == ValueKind::String);
        return strVal;
    }
    QString getVarVal() const {
        assert(kind == ValueKind::Ident);
        return strVal;
    }

    bool getBoolVal() const {
        assert(kind == ValueKind::Bool);
        return boolVal;
    }

    List* getListVal() const {
        assert(kind == ValueKind::LinkedList);
        return listVal;
    }

    Table* getTableVal() const {
        assert(kind == ValueKind::HashTable);
        return tableVal;
    }

    ValueKind getKind() const {
        return kind;
    }
    float getFloatVal() const {
        return floatVal;
    }

    void setFloatVal(float newFloatVal) {
        floatVal = newFloatVal;
    }

    Value(float val) : kind(ValueKind::Float), floatVal(val){};
    Value(long long val) : kind(ValueKind::Int), intVal(val){};
    Value(int val) : kind(ValueKind::Int), intVal(val){};
    Value(QString val) : kind(ValueKind::String), strVal(val){};
    Value(ValueKind _kind, QString _string)
        : kind(_kind), strVal(_string){};
    Value(bool val) : kind(ValueKind::Bool), boolVal(val){};
    Value(unsigned long val) : kind(ValueKind::Int), intVal(val){};
    Value(List* _node) : kind(ValueKind::LinkedList), listVal(_node) {
        std::cout << "Called new value constructor with list argument\n";
        listVal->data = new Value(0);
    }

    Value(Table* _element)
        : kind(ValueKind::HashTable), tableVal(_element) {
        std::cout << "Called new value constructor with table argument\n";
    }

    Value* newCopy() {
        auto result = new Value();
        *result     = *this;
        return result;
    }
};

struct Func {
    QString name;
    int     argCount;
    Func() = default;
    Func(QString _name, int _argc) : name(_name), argCount(_argc){};
};

enum class OPcode
{
    Load,
    Call,
    JumpIfFalse,
    Jump,
    Ass
};

struct Op {
    OPcode opcode;
    Value  arg;
    Func   func;
    Op() = default;
    Op(Value arg1) : opcode(OPcode::Load), arg(arg1){};
    Op(Func _func) : opcode(OPcode::Call), func(_func){};
    Op(OPcode _opcode) : opcode(_opcode){};
};

std::vector<std::pair<TokenKind, QRegularExpression>> Tokens{
    {TokenKind::FOR_KEY, QRegularExpression("for")},
    {TokenKind::NEW_KEY, QRegularExpression("new")},
    {TokenKind::WHILE_KEY, QRegularExpression("while")},
    {TokenKind::IF_KEY, QRegularExpression("if")},
    {TokenKind::ELSE_KEY, QRegularExpression("else")},
    {TokenKind::INC, QRegularExpression("\\+\\+")},
    {TokenKind::DEC, QRegularExpression("\\-\\-")},
    {TokenKind::LPar, QRegularExpression("\\(")},
    {TokenKind::RPar, QRegularExpression("\\)")},
    {TokenKind::LBrace, QRegularExpression("\\[")},
    {TokenKind::RBRace, QRegularExpression("\\]")},
    {TokenKind::LCurly, QRegularExpression("\\{")},
    {TokenKind::RCurly, QRegularExpression("\\}")},
    {TokenKind::SMCLN, QRegularExpression("\\;")},
    {TokenKind::DDOT, QRegularExpression("\\:")},
    {TokenKind::COMM, QRegularExpression("\\,")},
    {TokenKind::OP, QRegularExpression("[+-/*<>!&|]+")},
    {TokenKind::Ident, QRegularExpression("[a-zA-Z]+")},
    {TokenKind::ASSIGN_OP, QRegularExpression("=")},
    {TokenKind::FLOAT, QRegularExpression("[0-9]+\\.[0-9]+")},
    {TokenKind::NUMB, QRegularExpression("[0-9]+")},
    {TokenKind::SPACE, QRegularExpression("\\s+")}};

struct Token {
    QString   value;
    TokenKind type;
};

#define __OUT_AST(kind)                                                   \
    case AstKind::kind: {                                                 \
        return out << "" << #kind;                                        \
    }
std::ostream& operator<<(std::ostream& out, const AstKind kind) {
    switch (kind) {
        __OUT_AST(lang);
        __OUT_AST(stmt);
        __OUT_AST(while_stmt);
        __OUT_AST(if_stmt);
        __OUT_AST(assign_expr);
        __OUT_AST(expr);
        __OUT_AST(list);
        __OUT_AST(else_stmt);
        __OUT_AST(if_body);
        __OUT_AST(if_head);
        __OUT_AST(else_body);
        __OUT_AST(while_body);
        __OUT_AST(while_head);
        __OUT_AST(value);
        __OUT_AST(else_head);
        __OUT_AST(Ident);
        __OUT_AST(NUMB);
        __OUT_AST(LPar);
        __OUT_AST(OP);
        __OUT_AST(EQUAL);
        __OUT_AST(new_expr);
        __OUT_AST(call);
        __OUT_AST(FLOAT);
    }
}


#define __OUT_TOK(kind)                                                   \
    case TokenKind::kind: {                                               \
        return out << "" << #kind;                                        \
    }
std::ostream& operator<<(std::ostream& out, const TokenKind kind) {
    switch (kind) {
        __OUT_TOK(FOR_KEY);
        __OUT_TOK(NEW_KEY);
        __OUT_TOK(WHILE_KEY);
        __OUT_TOK(IF_KEY);
        __OUT_TOK(ELSE_KEY);
        __OUT_TOK(INC);
        __OUT_TOK(DEC);
        __OUT_TOK(LPar);
        __OUT_TOK(RPar);
        __OUT_TOK(LCurly);
        __OUT_TOK(RCurly);
        __OUT_TOK(LBrace);
        __OUT_TOK(RBRace);
        __OUT_TOK(SMCLN);
        __OUT_TOK(DDOT);
        __OUT_TOK(COMM);
        __OUT_TOK(OP);
        __OUT_TOK(Ident);
        __OUT_TOK(ASSIGN_OP);
        __OUT_TOK(NUMB);
        __OUT_TOK(SPACE);
        __OUT_TOK(FLOAT);
    }
}
QVector<Token>           matches;
QVector<Token>::Iterator match_it;

QVector<Token> lexer(QString code, bool printLex) {
    QVector<Token> matches;
    int            startpos = 0;

    while (startpos < code.length()) {
        for (const auto& pattern : Tokens) {
            QRegularExpressionMatch match = pattern.second.match(
                code, startpos);

            if (match.hasMatch()) {
                int pos = match.capturedStart(0);
                if (pos == startpos) {
                    QString found_piece = code.mid(
                        startpos, match.capturedLength());
                    if (pattern.first != TokenKind::SPACE)
                        matches.push_back({found_piece, pattern.first});
                    startpos += match.capturedLength();
                }
            }
        }
    }

    if (printLex) {
        for (int k = 0; k < matches.size(); k++) {
            std::cout << "| " << std::setw(5) << matches[k].type << " |"
                      << "    [" << matches[k].value.toStdString() << "]"
                      << std::endl;
        }
    }
    return matches;
}


class code_obj
{
  public:
    code_obj(AstKind type, QString value) {
        this->type  = type;
        this->value = value;
    }

    code_obj* get_child(int child_number) {
        return childs[child_number];
    }
    code_obj* get_parent() {
        return this->parent;
    }
    AstKind get_type() {
        return this->type;
    }

    void add_child(code_obj* child_obj) {
        childs.append(child_obj);
        child_obj->parent = this;
    }
    void set_parent(code_obj* parent) {
        parent->add_child(this);
    }
    QVector<code_obj*> get_childs() {
        return this->childs;
    }
    void print_tree(int level) {
        std::cout << level << ": ";
        for (int i = 0; i < level; i++)
            std::cout << "\t";
        std::cout << this->type;
        if (this->value != "")
            std::cout << "(" << this->value.toStdString() << ")";
        std::cout << "\n";
        if (!this->childs.isEmpty())
            for (it = childs.begin(); it != childs.end(); ++it) {
                (*it)->print_tree(level + 1);
            }
    }

    int get_prec() {
        if (this->value == "*") {
            return 3;
        } else if (this->value == "/") {
            return 3;
        } else if (this->value == "+") {
            return 2;
        } else if (this->value == "-") {
            return 2;
        } else if (this->value == "<") {
            return 1;
        } else if (this->value == ">") {
            return 1;
        } else {
            return 0;
        }
    }

    QString getValue() const;
    void    setValue(const QString& value);

  private:
    AstKind                      type;
    QString                      value = "";
    code_obj*                    parent;
    QVector<code_obj*>           childs;
    QVector<code_obj*>::Iterator it;
};

QStack<code_obj*> stack;

void parse_value(code_obj* value) {
    code_obj* new_obj;
    switch (match_it->type) {
        case TokenKind::NUMB: {
            new_obj = new code_obj(AstKind::NUMB, match_it->value);
            value->add_child(new_obj);
            break;
        } break;
        case TokenKind::Ident: {
            new_obj = new code_obj(AstKind::Ident, match_it->value);
            value->add_child(new_obj);
            break;
        } break;
        default: {
            return;
        }
    }
}

code_obj* parse_expr() {
    code_obj*      new_obj;
    QVector<Token> exprTokens;
    if (match_it->type == TokenKind::NEW_KEY) {
        match_it++;
        auto result = new code_obj(AstKind::new_expr, match_it->value);
        match_it++;
        return result;
    }
    if (match_it->type == TokenKind::LPar) {
        exprTokens.push_back(*match_it);
        match_it++;
        int balance = 1;
        while (balance > 0) {
            switch (match_it->type) {
                case TokenKind::LPar: {
                    ++balance;
                    break;
                }
                case TokenKind::RPar: {
                    --balance;
                    break;
                }
                default: break;
            }

            exprTokens.push_back(*match_it);
            match_it++;
        }
    } else {
        while (
            !((match_it->type == TokenKind::SMCLN)
              || (match_it->type == TokenKind::COMM)
              || match_it->type == TokenKind::RPar)) {
            exprTokens.push_back(*match_it);
            match_it++;
        }
    }


    std::map<QString, int> prec;
    prec["+"] = 2;
    prec["*"] = 1;
    prec["/"] = 1;
    prec["-"] = 2;
    QVector<Token> stack;
    QVector<Token> stack_res;
    for (const auto& token : exprTokens) {
        switch (token.type) {
            case TokenKind::OP: {
                while (!stack.empty()) {
                    if (prec[token.value] > prec[stack.back().value]) {
                        if (stack.back().type != TokenKind::LPar) {
                            stack_res.push_back(stack.back());
                        }
                        stack.pop_back();
                    } else {
                        break;
                    }
                }
                stack.push_back(token);
                break;
            }
            case TokenKind::LPar: {
                stack.push_back(token);
                break;
            }
            case TokenKind::RPar: {
                while (!stack.empty()
                       && stack.back().type != TokenKind::LPar) {
                    stack_res.push_back(stack.back());
                    stack.pop_back();
                }
                if (!stack.empty()) {
                    stack.pop_back();
                }
                break;
            }
            default: {
                stack_res.push_back(token);
            }
        }
    }
    while (!stack.empty()) {
        stack_res.push_back(stack.back());
        stack.pop_back();
    }
    QVector<code_obj*> evalStack;
    for (auto& token : stack_res) {
        if (token.type == TokenKind::NUMB) {
            evalStack.push_back(new code_obj(AstKind::NUMB, token.value));

        } else if (token.type == TokenKind::FLOAT) {
            evalStack.push_back(new code_obj(AstKind::FLOAT, token.value));

        } else if (token.type == TokenKind::Ident) {
            evalStack.push_back(new code_obj(AstKind::Ident, token.value));

        } else if (token.type == TokenKind::OP) {
            code_obj* lhs = evalStack.back();
            evalStack.pop_back();
            code_obj* rhs = evalStack.back();
            evalStack.pop_back();

            new_obj = new code_obj(AstKind::OP, token.value);
            new_obj->add_child(rhs);
            new_obj->add_child(lhs);
            evalStack.push_back(new_obj);
        }
    }

    return evalStack[0];
}

void parse_stmt(code_obj* expr);

void parse_stmt_block(code_obj* res) {
    MATCH(LCurly);
    while (OK(IF_KEY) || OK(WHILE_KEY) || OK(Ident)) {
        code_obj* new_obj = new code_obj(AstKind::stmt, "");
        res->add_child(new_obj);
        parse_stmt(new_obj);
        break;
    }
    MATCH(RCurly);
}

void parse_while(code_obj* while_stmt) {
    MATCH(WHILE_KEY);

    code_obj* new_obj;
    new_obj = new code_obj(AstKind::while_head, "");
    new_obj->add_child(parse_expr());
    while_stmt->add_child(new_obj);

    new_obj = new code_obj(AstKind::while_body, "");
    parse_stmt_block(new_obj);
    while_stmt->add_child(new_obj);
}


void parse_if(code_obj* if_stmt) {
    code_obj* new_obj;
    MATCH(IF_KEY);
    if_stmt->add_child(parse_expr());

    new_obj = new code_obj(AstKind::if_body, "");
    parse_stmt_block(new_obj);
    if_stmt->add_child(new_obj);
    if (match_it->type == TokenKind::ELSE_KEY) {
        MATCH(ELSE_KEY);
        code_obj* new_obj = new code_obj(AstKind::else_body, "");
        parse_stmt_block(new_obj);
        if_stmt->add_child(new_obj);
    }
}


void parse_assign_list(code_obj* assign_stmt_list) {
}

void parse_call(code_obj* call) {
    MATCH(Ident);
    MATCH(LPar);
    while (OK(NUMB) || OK(Ident) || OK(LPar)) {
        call->add_child(parse_expr());
        if (OK(COMM)) {
            match_it++;
        } else {
            break;
        }
    }
    MATCH(RPar);
}


void parse_assign(code_obj* assign_stmt) {
    assign_stmt->add_child(new code_obj(AstKind::Ident, match_it->value));
    MATCH(Ident);
    MATCH(ASSIGN_OP);

    if (match_it->type == TokenKind::Ident) {
        match_it++;
        if (match_it->type == TokenKind::LPar) {
            --match_it;
            code_obj* call = new code_obj(AstKind::call, match_it->value);
            parse_call(call);
            assign_stmt->add_child(call);

        } else {
            --match_it;
            assign_stmt->add_child(parse_expr());
        }
    } else {
        assign_stmt->add_child(parse_expr());
    }

    MATCH(SMCLN);
}


void parse_stmt(code_obj* expr) {
    if (!(OK(Ident) || OK(WHILE_KEY) || OK(IF_KEY))) {
        throw "FUCK";
    }
    switch (match_it->type) {
        case TokenKind::WHILE_KEY: {
            code_obj* new_obj = new code_obj(AstKind::while_stmt, "");
            parse_while(new_obj);
            expr->add_child(new_obj);
            break;
        } break;
        case TokenKind::IF_KEY: {
            code_obj* new_obj = new code_obj(AstKind::if_stmt, "");
            parse_if(new_obj);
            expr->add_child(new_obj);
            break;
        } break;
        case TokenKind::Ident: {
            match_it++;

            if (match_it->type == TokenKind::LPar) {
                --match_it;
                code_obj* new_obj = new code_obj(
                    AstKind::call, match_it->value);
                parse_call(new_obj);
                expr->add_child(new_obj);
                MATCH(SMCLN);

            } else {
                --match_it;
                code_obj* new_obj = new code_obj(AstKind::assign_expr, "");
                parse_assign(new_obj);
                expr->add_child(new_obj);
            }

            break;
        } break;
    }
}

void parse_lang(code_obj* lang) {
    while (match_it < matches.end()
           && (OK(IF_KEY) || OK(WHILE_KEY) || OK(Ident))) {
        code_obj* new_obj = new code_obj(AstKind::stmt, "");
        parse_stmt(new_obj);
        lang->add_child(new_obj);
    }
}

QVector<Op> to_polish(code_obj* root) {
    QVector<Op> result;
    switch (root->get_type()) {
        case AstKind::NUMB: {
            result.push_back(Op(Value(root->getValue().toInt())));
        } break;
        case AstKind::Ident: {
            result.push_back(
                Op(Value(ValueKind::Ident, root->getValue())));
        } break;
        case AstKind::FLOAT: {
            result.push_back(Op(Value(root->getValue().toFloat())));
        } break;

        case AstKind::call:
        case AstKind::OP: {
            for (code_obj* node : root->get_childs()) {
                result.append(to_polish(node));
            }
            result.push_back(
                Op(Func(root->getValue(), root->get_childs().size())));
            break;
        }

        case AstKind::assign_expr: {
            for (const auto& ob : root->get_childs()) {
                result.append(to_polish(ob));
            }
            result.push_back(Op(OPcode::Ass));
            break;
        } break;

        case AstKind::value:
        case AstKind::lang:
        case AstKind::expr:
        case AstKind::stmt:
        case AstKind::else_stmt:
        case AstKind::if_head:
        case AstKind::else_head:
        case AstKind::if_body:
        case AstKind::else_body:
        case AstKind::while_head:
        case AstKind::while_body: {
            for (const auto& ob : root->get_childs()) {
                result.append(to_polish(ob));
            }
            break;
        }
        case AstKind::if_stmt: {
            auto condition = to_polish(root->get_child(0));
            auto body      = to_polish(root->get_child(1));

            result.append(condition);
            result.push_back(Op(Value(body.size() + 1)));
            result.push_back(Op(OPcode::JumpIfFalse));

            result.append(body);
            break;
        }
        case AstKind::while_stmt: {
            auto condition = to_polish(root->get_child(0));
            auto body      = to_polish(root->get_child(1));
            result.append(condition);
            result.push_back(Op(Value(body.size() + 3)));
            result.push_back(Op(OPcode::JumpIfFalse));
            result.append(body);
            result.push_back(
                Op(Value(-(condition.size() + body.size() + 3))));
            result.push_back(Op(OPcode::Jump));
            break;
        }
        case AstKind::new_expr: {
            if (root->getValue() == "LinkedList") {
                result.push_back(Op(Value(new List())));
            } else if (root->getValue() == "HashTable") {
                result.push_back(Op(Value(new Table())));
            }
        }
    }
    return result;
}
std::ostream& operator<<(std::ostream& os, const Value& value);

std::ostream& operator<<(std::ostream& os, List* value) {
    auto ptr = value;
    while (ptr != nullptr) {
        if (ptr->data == nullptr) {
            os << "NO DATA";
        } else {
            os << "->" << *ptr->data;
        }
        ptr = ptr->next;
    }

    return os;
}
std::ostream& operator<<(std::ostream& os, const Value& value) {
    os << "[";
    switch (value.getKind()) {
        case ValueKind::Int: {
            os << "Int " << value.getIntVal();
            break;
        }
        case ValueKind::Ident: {
            os << "Var " << value.getVarVal().toStdString();
            break;
        }
        case ValueKind::String: {
            os << "String " << value.getStrVal().toStdString();
            break;
        }
        case ValueKind::Float: {
            os << "Float " << value.getFloatVal();
            break;
        }
        case ValueKind::Bool: {
            os << "Bool ";
            if (value.getBoolVal()) {
                os << "true";
            } else {
                os << "false";
            }
            break;
        }
        case ValueKind::LinkedList: {
            os << "List ";
            os << value.getListVal();
            break;
        }
        case ValueKind::HashTable: {
            os << "Table ";
            int cnt = 0;
            for (const auto& b : value.getTableVal()->bucket) {
                for (const auto& p : b.entries) {
                    if (cnt > 0) {
                        os << ", ";
                    }
                    os << "{" << *p.key << ": " << *p.value << "}";
                    ++cnt;
                }
            }
            break;
        }
    }

    os << "]";

    return os;
}

std::ostream& operator<<(std::ostream& os, OPcode op) {
    switch (op) {
        case OPcode::Load: return os << "Load";
        case OPcode::Call: return os << "Call";
        case OPcode::JumpIfFalse: return os << "JumpIfFalse";
        case OPcode::Jump: return os << "Jump";
        case OPcode::Ass: return os << "Ass";
    }
}

std::ostream& operator<<(std::ostream& os, Op op) {
    os << op.opcode << " ";
    switch (op.opcode) {
        case OPcode::Load: {
            os << op.arg;
            break;
        }
        case OPcode::Call: {
            os << "[ " << op.func.name.toStdString() << " ]"
               << "(" << op.func.argCount << ")";
            break;
        }
        case OPcode::Ass: {
            break;
        }
        default: break;
    }

    return os;
}

#define LIFT_OP(OP)                                                       \
    switch (args[0].getKind()) {                                          \
        case ValueKind::Int: {                                            \
            switch (args[1].getKind()) {                                  \
                case ValueKind::Int:                                      \
                    return Value(                                         \
                        args[0].getIntVal() OP args[1].getIntVal());      \
                case ValueKind::Float:                                    \
                    return Value(                                         \
                        args[0].getIntVal() OP args[1].getFloatVal());    \
                default: throw "ERROR";                                   \
            }                                                             \
        } break;                                                          \
        case ValueKind::Float: {                                          \
            switch (args[1].getKind()) {                                  \
                case ValueKind::Int:                                      \
                    return Value(                                         \
                        args[0].getFloatVal() OP args[1].getIntVal());    \
                case ValueKind::Float:                                    \
                    return Value(                                         \
                        args[0].getFloatVal() OP args[1].getFloatVal());  \
                default: throw "ERROR";                                   \
            }                                                             \
        } break;                                                          \
        default: throw "ERROR";                                           \
    }



std::optional<Value> evalFunc(QString name, QVector<Value> args) {
    if (name == "+") {
        LIFT_OP(+);

    } else if (name == "*") {
        LIFT_OP(*);

    } else if (name == "/") {
        LIFT_OP(/);

    } else if (name == "-") {
        LIFT_OP(-);

    } else if (name == "==") {
        return Value(args[0].getIntVal() == args[1].getIntVal());


    } else if (name == "print") {
        std::cout << "print: ";
        for (const auto& arg : args) {
            std::cout << arg << " ";
        }
        std::cout << std::endl;
    } else if (name == "<") {
        return Value(args[0].getIntVal() < args[1].getIntVal());

    } else if (name == ">") {
        return Value(args[0].getIntVal() > args[1].getIntVal());

    } else if (name == "add") {
        args[0].getListVal()->insert(args[1].newCopy());

    } else if (name == "get") {
        return *args[0].getListVal()->get(args[1].getIntVal());

    } else if (name == "set") {
        args[0].getTableVal()->insert(
            args[1].newCopy(), args[2].newCopy());

    } else if (name == "receive") {
        auto res = args[0].getTableVal()->get_table(args[1].newCopy());

        return *res;
    }

    return std::nullopt;
};

std::pair<QMap<QString, Value>, QVector<Value>> eval_code(
    QVector<Op> program,
    bool        printRun) {
    int                  programCounter = 0;
    QMap<QString, Value> var_table;
    QVector<Value>       stack;

    while (programCounter < program.size()) {
        auto cmd = program[programCounter];
        if (printRun) {
            std::cout << std::left << "@" << std::setw(4) << programCounter
                      << cmd << std::endl;
        }

        switch (cmd.opcode) {
            case OPcode::Load: {
                stack.push_back(cmd.arg);
                ++programCounter;
                break;
            }
            case OPcode::Call: {
                QVector<Value> args;
                for (int i = 0; i < cmd.func.argCount; ++i) {
                    auto value = stack.back();
                    if (value.getKind() == ValueKind::Ident) {
                        value = var_table[value.getVarVal()];
                    }
                    args.push_back(value);
                    stack.pop_back();
                }

                std::reverse(args.begin(), args.end());

                auto res = evalFunc(cmd.func.name, args);
                if (res.has_value()) {
                    stack.push_back(res.value());
                }
                ++programCounter;
                break;
            }
            case OPcode::Ass: {
                auto value = stack.back();
                stack.pop_back();
                var_table[stack.back().getVarVal()] = value;
                stack.pop_back();
                ++programCounter;
                break;
            }
            case OPcode::JumpIfFalse: {
                auto jump_address = stack.back();
                stack.pop_back();
                auto condition = stack.back();
                stack.pop_back();
                if (condition.getBoolVal() == true) {
                    ++programCounter;
                } else {
                    programCounter = programCounter
                                     + jump_address.getIntVal();
                }
                break;
            }
            case OPcode::Jump: {
                auto jump_address = stack.back();
                stack.pop_back();
                programCounter = programCounter + jump_address.getIntVal();
                break;
            }
        }
    }
    return {var_table, stack};
}

void Table::insert(Value* key, Value* value, bool recount) {
    auto idx = key->hash() % this->bucket.size();
    if (elements_amount * 2 > this->bucket.size()) {
        resize();
    }
    if (bucket[idx].entries.size() == 0) {
        bucket[idx].entries.push_back({key, value});
        if (recount) {
            elements_amount++;
        }
    } else {
        for (int pair_idx = 0; pair_idx < this->bucket[idx].entries.size();
             ++pair_idx) {
            if (*bucket[idx].entries[pair_idx].key == *key) {
                bucket[idx].entries[pair_idx].value = value;
                return;
            }
        }
        bucket[idx].entries.push_back({key, value});
        if (recount) {
            elements_amount++;
        }
    }
}

Value* Table::get_table(Value* key) {
    auto hash = key->hash();
    auto idx  = hash % this->bucket.size();
    for (int pair_idx = 0; pair_idx < this->bucket[idx].entries.size();
         ++pair_idx) {
        if (this->bucket[idx].entries.size() == 0) {
            std::cerr << "The bucket is empty" << std::endl;
            throw "Empty bucket";
        } else if (*this->bucket[idx].entries[pair_idx].key == *key) {
            return bucket[idx].entries[pair_idx].value;
        }
    }
    std::cerr << "No matching key " << *key << std::endl;
    abort();
}

void Table::resize() {
    auto old_bucket = this->bucket;
    bucket          = QVector<Bucket>(this->bucket.size() * 2);
    std::cout << "Resize to " << bucket.size() << " buckets " << std::endl;
    for (auto& old : old_bucket) {
        for (auto& e : old.entries) {
            this->insert(e.key, e.value, false);
        }
    }
}

bool operator==(const Value& a, const Value& b) {
    bool result = a.kind == b.kind;
    if (result) {
        switch (a.kind) {
            case ValueKind::Int: return a.intVal == b.intVal;
            case ValueKind::String: return a.strVal == b.strVal;
        }
    }
    return result;
}

struct TestConfig {
    QString code;
    bool    printAst;
    bool    printLex;
    bool    printPolish;
    bool    printEval;
    bool    printResult;
};

int main(int argc, char* argv[]) {

    std::vector<TestConfig> tests = {
        {"if (5 < 6) {a = 7 + b;}", true,true, true, true, true},
        {"a = ((((((a + b))))));", true, true, true, true, true},
        {"b = 5; while( b > 3) {b = b - 1;}", true, true, true, true, true},
        {"x = new HashTable; set(x, 4, 99); set(x, 6, 25); y = receive(x, 6);",
         true,
         true,
         true,
         true,
         true},
        {"z = new LinkedList; add(z, 4); add(z, 67); add(z, 5); q = "
         "get(z, 1);",
         true,
         true,
         true,
         true,
         true}

    };


    for (const auto S : tests) {
        std::cout << "\033[41mTESTING\033[0m [" << S.code.toStdString()
                  << "]\n";
        std::cout << "---------------------------------------------\n";
        matches  = lexer(S.code, S.printLex);
        match_it = matches.begin();
        code_obj lang(AstKind::lang, "");

        parse_lang(&lang);
        if (S.printAst) {
            std::cout << "AST:"<< std::endl;
            lang.print_tree(0);
            std::cout << std::endl;
        }

        QVector<Op> program = to_polish(&lang);
        if (S.printPolish) {
            std::cout << "POLISH and Stack-machine:"<< std::endl;
            for (int i = 0; i < program.size(); ++i) {
                std::cout << std::left << std::setw(6) << i << " "
                          << program[i] << std::endl;
            }
        }


        auto result = eval_code(program, S.printEval);
        if (S.printResult) {
            std::cout << "--- variable table ---\n";
            for (const auto& key : result.first.keys()) {
                std::cout << key.toStdString() << " = "
                          << result.first[key] << "\n";
            }

            std::cout << "--- stack values---\n";
            for (const auto& val : result.second) {
                std::cout << val << "\n";
            }
        }
    }
    return 0;
}

QString code_obj::getValue() const {
    return value;
}
