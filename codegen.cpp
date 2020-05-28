#include "node.h"
#include "codegen.h"
#include "parser.hpp"

using namespace std;

/* Compile the AST into a module */
void CodeGenContext::generateCode(NBlock& root)
{
	std::cout << "Generating code...\n";
	
	/* Create the top level interpreter function to call as entry */
	vector<Type*> argTypes;
	FunctionType *ftype = FunctionType::get(Type::getVoidTy(MyContext), makeArrayRef(argTypes), false);
	mainFunction = Function::Create(ftype, GlobalValue::ExternalLinkage, "main", module);
	BasicBlock *bblock = BasicBlock::Create(MyContext, "entry", mainFunction, 0);
	
	/* Push a new variable/block context */
	pushBlock(bblock);
	root.codeGen(*this); /* emit bytecode for the toplevel block */

	ReturnInst::Create(MyContext, this->currentBlock());//当前块，不再是bblock，块已经分离过了
	popBlock();
	
	/* Print the bytecode in a human-readable format 
	   to see if our program compiled properly
	 */
	std::cout << "Code is generated.\n";
	// module->dump();

	legacy::PassManager pm;
	pm.add(createPrintModulePass(outs()));
	pm.run(*module);
}

/* Executes the AST by running the main function */
GenericValue CodeGenContext::runCode() {
	std::cout << "Running code...\n";
	ExecutionEngine *ee = EngineBuilder( unique_ptr<Module>(module) ).create();
	ee->finalizeObject();
	vector<GenericValue> noargs;
	GenericValue v = ee->runFunction(mainFunction, noargs);
	std::cout << "Code was run.\n";
	return v;
}

/* Returns an LLVM type based on the identifier */
static Type *typeOf(const NIdentifier& type) 
{
	if (type.name.compare("int") == 0) {
		return Type::getInt64Ty(MyContext);
	}
	else if (type.name.compare("double") == 0) {
		return Type::getDoubleTy(MyContext);
	}
	return Type::getVoidTy(MyContext);
}

/* -- Code Generation -- */

Value* NInteger::codeGen(CodeGenContext& context)
{
	std::cout << "Creating integer: " << value << endl;
	
	return ConstantInt::get(Type::getInt64Ty(MyContext), value, true);
}


Value* NDouble::codeGen(CodeGenContext& context)
{
	std::cout << "Creating double: " << value << endl;
	return ConstantFP::get(Type::getDoubleTy(MyContext), value);
}

Value* NIdentifier::codeGen(CodeGenContext& context)
{
	std::cout << "Creating identifier reference: " << name << endl;
	if (context.locals().find(name) == context.locals().end()) {
		if(context.locals().find(name+"__PASCAL__RET")  == context.locals().end()){
			std::cerr << "undeclared variable " << name << endl;
			return NULL;
		}
		else{
			return new LoadInst(context.locals()[name+"__PASCAL__RET"], "", false, context.currentBlock());
		}
	}
	else{
		return new LoadInst(context.locals()[name], "", false, context.currentBlock());
	}
}

Value* NMethodCall::codeGen(CodeGenContext& context)
{
	Function *function = context.module->getFunction(id.name.c_str());
	if (function == NULL) {
		std::cerr << "no such function " << id.name << endl;
	}
	std::vector<Value*> args;
	ExpressionList::const_iterator it;
	for (it = arguments.begin(); it != arguments.end(); it++) {
		args.push_back((**it).codeGen(context));
	}
	CallInst *call = CallInst::Create(function, makeArrayRef(args), "", context.currentBlock());
	std::cout << "Creating method call: " << id.name << endl;
	return call;
}

Value* NBinaryOperator::codeGen(CodeGenContext& context)
{
	std::cout << "Creating binary operation " << op << endl;
	Instruction::BinaryOps instr;
	int swap_pos=0;
	switch (op) {
		case TPLUS: 	instr = Instruction::Add; goto math;
		case TMINUS: 	instr = Instruction::Sub; goto math;
		case TMUL: 		instr = Instruction::Mul; goto math;
		case TDIV: 		instr = Instruction::SDiv; goto math;
		/* TODO comparison */
		case TCLT:		swap_pos=1;instr = Instruction::Sub; goto math;
		case TCLE:		swap_pos=1;instr = Instruction::Sub; goto math;
		case TCGT:		instr = Instruction::Sub; goto math;
		case TCGE:		instr = Instruction::Sub; goto math;
	}
	
	return NULL;
math:
	if (swap_pos==1){
		return BinaryOperator::Create(instr, rhs.codeGen(context), 
			lhs.codeGen(context), "", context.currentBlock());
	}
	else{
		return BinaryOperator::Create(instr, lhs.codeGen(context), 
		rhs.codeGen(context), "", context.currentBlock());
	}

}

Value* NAssignment::codeGen(CodeGenContext& context)
{
	std::cout << "Creating assignment for " << lhs.name << endl;
	if (context.locals().find(lhs.name) == context.locals().end()) {
		if(context.locals().find(lhs.name+"__PASCAL__RET") ==context.locals().end()){
			std::cerr << "undeclared variable " << lhs.name << endl;
			return NULL;		
		}
		else{
			return new StoreInst(rhs.codeGen(context), context.locals()[lhs.name+"__PASCAL__RET"], false, context.currentBlock());

		}
	}
	else{
		return new StoreInst(rhs.codeGen(context), context.locals()[lhs.name], false, context.currentBlock());
	}
}

Value* NBlock::codeGen(CodeGenContext& context)
{
	StatementList::const_iterator it;
	Value *last = NULL;
	for (it = statements.begin(); it != statements.end(); it++) {
		std::cout << "Generating code for " << typeid(**it).name() << endl;
		last = (**it).codeGen(context);
	}
	std::cout << "Creating block" << endl;
	return last;
}

Value* NExpressionStatement::codeGen(CodeGenContext& context)
{
	std::cout << "Generating code for " << typeid(expression).name() << endl;
	return expression.codeGen(context);
}

Value* NReturnStatement::codeGen(CodeGenContext& context)
{
	std::cout << "Generating return code for " << typeid(expression).name() << endl;
	Value *returnValue = expression.codeGen(context);
	context.setCurrentReturnValue(returnValue);
	return returnValue;
}

Value* NVariableDeclaration::codeGen(CodeGenContext& context)
{
	std::cout << "Creating variable declaration " << type.name << " " << id.name << endl;
	AllocaInst *alloc = new AllocaInst(typeOf(type), id.name.c_str(), context.currentBlock());
	context.locals()[id.name] = alloc;
	if (assignmentExpr != NULL) {
		NAssignment assn(id, *assignmentExpr);
		assn.codeGen(context);
	}
	return alloc;
}


Value* NVariableDeclarationS::codeGen(CodeGenContext& context)
{
	std::cout << "Creating variable declarationS " <<std::endl;
	Value* ret;
	for(int i=0;i<VariableDeclarationList.size();i++){
		ret=VariableDeclarationList[i]->codeGen(context);
	}
	return ret;

}

Value* NExternDeclaration::codeGen(CodeGenContext& context)
{
    vector<Type*> argTypes;
    VariableList::const_iterator it;
    for (it = arguments.begin(); it != arguments.end(); it++) {
        argTypes.push_back(typeOf((**it).type));
    }
    FunctionType *ftype = FunctionType::get(typeOf(type), makeArrayRef(argTypes), false);
    Function *function = Function::Create(ftype, GlobalValue::ExternalLinkage, id.name.c_str(), context.module);
    return function;
}

Value* NFunctionDeclaration::codeGen(CodeGenContext& context)
{
	vector<Type*> argTypes;
	VariableList::const_iterator it;
	for (it = arguments.begin(); it != arguments.end(); it++) {
		argTypes.push_back(typeOf((**it).type));
	}
	FunctionType *ftype = FunctionType::get(typeOf(type), makeArrayRef(argTypes), false);
	Function *function = Function::Create(ftype, GlobalValue::InternalLinkage, id.name.c_str(), context.module);
	BasicBlock *bblock = BasicBlock::Create(MyContext, "entry", function, 0);
	
	context.pushBlock(bblock);
	

	Function::arg_iterator argsValues = function->arg_begin();
    Value* argumentValue;

	for (it = arguments.begin(); it != arguments.end(); it++) {
		(**it).codeGen(context);
		
		argumentValue = &*argsValues++;
		argumentValue->setName((*it)->id.name.c_str());
		StoreInst *inst = new StoreInst(argumentValue, context.locals()[(*it)->id.name], false, bblock);
	}

	//中间构造返回值
	
	NIdentifier* ret_ident=new NIdentifier(id.name + "__PASCAL__RET");
	block.statements.push_back( new NReturnStatement(*ret_ident)); //返回操作加入到block中

	NVariableDeclaration *ret_define=new NVariableDeclaration( type, *ret_ident   );
	ret_define->codeGen(context);

	//返回值构造结束

	block.codeGen(context);
	ReturnInst::Create(MyContext, context.getCurrentReturnValue(), context.currentBlock());

	context.popBlock();
	std::cout << "Creating function: " << id.name << endl;
	return function;
}

Value* NIFStatement::codeGen(CodeGenContext& context)
{
			
	Value *CondV = condition_expr.codeGen(context);
	
    if (CondV == 0)return 0;
	Function *TheFunction=context.currentBlock()->getParent();
	BasicBlock *ThenBB,*ElseBB,*MergeBB;
	ThenBB =BasicBlock::Create(MyContext, "then",TheFunction);

	//产生else block
	if(else_use==1)ElseBB = BasicBlock::Create(MyContext, "else",TheFunction);

	MergeBB = BasicBlock::Create(MyContext, "ifcont",TheFunction);
	llvm:IRBuilder<> Builder(MyContext);
	auto *IntType = Builder.getInt64Ty();
	ICmpInst *icmp = new ICmpInst(*context.currentBlock(),ICmpInst::ICMP_SLT,ConstantInt::get(IntType,0),CondV );	

	//是否包括分支到else
	if (else_use==1)BranchInst::Create	(	ThenBB, ElseBB,icmp,context.currentBlock() );	
	else BranchInst::Create	(	ThenBB, MergeBB,icmp,context.currentBlock() );	

	std::map<std::string, Value*> templocals,templocals_1,templocals_2;
	templocals=context.locals();
	context.popBlock();
	context.pushBlock(ThenBB);
	context.setLocals(templocals);
	
	then_block.codeGen(context);
	BranchInst::Create(MergeBB,context.currentBlock());


	templocals_1=context.locals();
	context.popBlock();

	//else 代码生成
	if(else_use){
		context.pushBlock(ElseBB);
		context.setLocals(templocals_1);
		else_block.codeGen(context);
		BranchInst::Create(MergeBB,context.currentBlock());
		templocals_2=context.locals();
		context.popBlock();
	}
	context.pushBlock(MergeBB);
	context.setLocals(templocals_2);

    return CondV;

}


Value* FORStatement::codeGen(CodeGenContext& context)
{
	
	//循环变量赋初值
	NAssignment* inintal_assign= new NAssignment( iter,condition_start );
	inintal_assign->codeGen(context);
	NInteger *iter_add_value = new NInteger(1);

	Function *TheFunction=context.currentBlock()->getParent();

	BasicBlock *judgeBB=BasicBlock::Create(MyContext, "judge",TheFunction);
	BasicBlock *workBB=BasicBlock::Create(MyContext, "work",TheFunction);
	BasicBlock *endBB=BasicBlock::Create(MyContext, "end",TheFunction);

	BranchInst::Create	(	judgeBB, context.currentBlock() );
	std::map<std::string, Value*> templocals,templocals_1,templocals_2;

	templocals=context.locals();
	context.popBlock();
	context.pushBlock(judgeBB);
	context.setLocals(templocals);

	LoadInst *lditer = new LoadInst(context.locals()[iter.name], "", false, context.currentBlock());
	ICmpInst *icmp = new ICmpInst(*context.currentBlock(), ICmpInst::ICMP_SLE , lditer, condition_end.codeGen(context) );	

	BranchInst::Create	(	workBB, endBB, icmp ,context.currentBlock() );	

	templocals_1=context.locals();
	context.popBlock();
	context.pushBlock(workBB);
	context.setLocals(templocals_1);

	for_block.codeGen(context);

	
	NBinaryOperator* additer=new NBinaryOperator(iter, TPLUS,  *iter_add_value );
	NAssignment *add_one_to_iter = new NAssignment(iter,*additer);
	add_one_to_iter->codeGen(context);
	BranchInst::Create	(	judgeBB, context.currentBlock() );

	templocals_2=context.locals();
	context.popBlock();
	context.pushBlock(endBB);
	context.setLocals(templocals_2);

}