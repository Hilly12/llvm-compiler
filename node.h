#include <llvm/IR/Value.h>

#include <iostream>
#include <vector>

using namespace std;

class CodeGenContext;
class NStatement;
class NExpression;
class NVariableDeclaration;

class Node {
   public:
    virtual ~Node() {}
    virtual llvm::Value *codeGen(CodeGenContext &context) {}
};

class NProgram : public Node {
   public:
    vector<NClass *> classes;
    NProgram() {}
    virtual llvm::Value *codeGen(CodeGenContext &context);
};

class NClass : public Node {
   public:
    vector<NVariableDeclaration *> variables;
    vector<NFunctionDeclaration *> functions;
    NClass() {}
    virtual llvm::Value *codeGen(CodeGenContext &context);
};

class NExpression : public Node {};

class NStatement : public Node {};

class NInteger : public NExpression {
   public:
    long long value;
    NInteger(long long value) : value(value) {}
    virtual llvm::Value *codeGen(CodeGenContext &context);
};

class NDouble : public NExpression {
   public:
    double value;
    NDouble(double value) : value(value) {}
    virtual llvm::Value *codeGen(CodeGenContext &context);
};

class NIdentifier : public NExpression {
   public:
    std::string name;
    NIdentifier(const std::string &name) : name(name) {}
    virtual llvm::Value *codeGen(CodeGenContext &context);
};

class NMethodCall : public NExpression {
   public:
    const NIdentifier &id;
    vector<NExpression *> arguments;
    NMethodCall(const NIdentifier &id, vector<NExpression *> &arguments)
        : id(id), arguments(arguments) {}
    NMethodCall(const NIdentifier &id) : id(id) {}
    virtual llvm::Value *codeGen(CodeGenContext &context);
};

class NBinaryOperator : public NExpression {
   public:
    int op;
    NExpression &lhs;
    NExpression &rhs;
    NBinaryOperator(NExpression &lhs, int op, NExpression &rhs)
        : lhs(lhs), rhs(rhs), op(op) {}
    virtual llvm::Value *codeGen(CodeGenContext &context);
};

class NAssignment : public NExpression {
   public:
    NIdentifier &lhs;
    NExpression &rhs;
    NAssignment(NIdentifier &lhs, NExpression &rhs) : lhs(lhs), rhs(rhs) {}
    virtual llvm::Value *codeGen(CodeGenContext &context);
};

class NBlock : public NExpression {
   public:
    vector<NStatement *> statements;
    NBlock() {}
    virtual llvm::Value *codeGen(CodeGenContext &context);
};

class NExpressionStatement : public NStatement {
   public:
    NExpression &expression;
    NExpressionStatement(NExpression &expression) : expression(expression) {}
    virtual llvm::Value *codeGen(CodeGenContext &context);
};

class NVariableDeclaration : public NStatement {
   public:
    const NIdentifier &type;
    NIdentifier &id;
    NExpression *assignmentExpr;
    NVariableDeclaration(const NIdentifier &type, NIdentifier &id)
        : type(type), id(id) {}
    NVariableDeclaration(const NIdentifier &type, NIdentifier &id,
                         NExpression *assignmentExpr)
        : type(type), id(id), assignmentExpr(assignmentExpr) {}
    virtual llvm::Value *codeGen(CodeGenContext &context);
};

class NFunctionDeclaration : public NStatement {
   public:
    const NIdentifier &type;
    const NIdentifier &id;
    vector<NVariableDeclaration *> arguments;
    NBlock &block;
    NFunctionDeclaration(const NIdentifier &type, const NIdentifier &id,
                         const vector<NVariableDeclaration *> &arguments,
                         NBlock &block)
        : type(type), id(id), arguments(arguments), block(block) {}
    virtual llvm::Value *codeGen(CodeGenContext &context);
};