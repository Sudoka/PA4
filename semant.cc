

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}

typedef SymbolTable<Symbol, SymData>& MySymTable;

/* create symbol table */

ClassTable::ClassTable(Classes classes)
: semant_errors(0) , error_stream(cerr) {
    m_class_symtable.enterscope();

    /* Fill this in */
    install_basic_classes();

    for ( int i = classes->first(); classes->more(i); i = classes->next(i) ) {
        class__class* class_ = static_cast<class__class*>(classes->nth(i));
        if ( m_class_symtable.probe(class_->getName()) != NULL ) {
            ostream& os = semant_error(class_);
            os << "Class" << class_->getName() << " was previously defined." << endl;
        }
        SymData* symdata = new SymData(ClassType, class_, class_->getName());
        m_class_symtable.addid(class_->getName(), symdata);
    }

    for ( int i = classes->first(); classes->more(i); i = classes->next(i) ) {
        class__class* class_ = static_cast<class__class*>(classes->nth(i));
        semant_class(class_);
    }

    SymData* class_symdata = m_class_symtable.probe(Main);
    if ( class_symdata == NULL ) {
        ostream& os = semant_error();
        os << "Class Main is not defined." << endl;
    }
    else {
        MySymTable symtable = class_symdata->m_class->getSymTable();
        if ( symtable.probe(main_meth) == NULL ) {
            ostream& os = semant_error(class_symdata->m_class);
            os << "No 'main' method in class Main." << endl;
        }
    }

    m_class_symtable.exitscope();
}

void ClassTable::semant_class(class__class* class_) {
    if ( class_->getName() != Object && class_->getName() != No_type ) {
        if ( m_class_symtable.probe(class_->getParent()) == NULL ) {
            ostream& os = semant_error(class_);
            os << "Class" << class_->getName() << " inherits from an undefined class " << class_->getParent() << "." << endl;
        }
    }

    MySymTable symtable = class_->getSymTable();
    symtable.enterscope();
    Features features = class_->getFeatures();
    for ( int i = features->first(); features->more(i); i = features->next(i) ) {
        Feature feature = features->nth(i);
        if ( feature->getType() == AttrType ) {
            semant_attr(class_, feature);
        }
    }
    for ( int i = features->first(); features->more(i); i = features->next(i) ) {
        Feature feature = features->nth(i);
        if ( feature->getType() == MethodType ) {
            semant_method(class_, feature);
        }
    }
}

void ClassTable::semant_attr(class__class* class_, Feature feature) {
    attr_class* attr = static_cast<attr_class*>(feature);
    Symbol attrname = attr->getName();
    MySymTable symtable = class_->getSymTable();
    if ( symtable.probe(attrname) != NULL ) {
        ostream& os = semant_error(class_);
        os << "Attribute " << attrname << " is multiply defined in class." << endl;
    }

    Symbol declaretype = attr->getDeclareType();
    if ( m_class_symtable.lookup(declaretype) == NULL ) {
        ostream& os = semant_error(class_);
        os << "Class " << declaretype << " of attribute " << attrname << " is undefined." << endl;
    }

    semant_expression(class_, attr->getExpression());

    SymData* symdata = new SymData(AttrType, class_, declaretype);
    symtable.addid(attr->getName(), symdata);
}

void ClassTable::semant_method(class__class* class_, Feature feature) {
    method_class* method = static_cast<method_class*>(feature);
    Symbol methodname = method->getName();
    MySymTable symtable = class_->getSymTable();
    if ( symtable.probe(methodname) != NULL ) {
        ostream& os = semant_error(class_);
        os << "Method" << methodname << " is multiply defined." << endl;
    }

    Symbol returntype = method->getReturnType();
    if ( m_class_symtable.lookup(returntype) == NULL ) {
        ostream& os = semant_error(class_);
        os << "Undefined return type " << returntype << " in method " << methodname << "." << endl;
    }

    SymData* method_data = new SymData(MethodType, class_, returntype);

    symtable.enterscope();
    Formals formals = method->getFormals();
    for ( int i = formals->first(); formals->more(i); i = formals->next(i) ) {
        semant_formal(class_, method_data, static_cast<formal_class*>(formals->nth(i)));
    }

    // FIXME
    //semant_method_expr();
    //define method first or we can't find method name
    Expression expr = method->getExpression();
    semant_expression(class_, expr);
    // end

    symtable.exitscope();
    symtable.addid(method->getName(), method_data);
}

void ClassTable::semant_formal(class__class* class_, SymData* method_data, formal_class* formal) {
    Symbol formalname = formal->getName();
    MySymTable symtable = class_->getSymTable();
    if ( symtable.probe(formalname) != NULL ) {
        ostream& os = semant_error(class_);
        os << "Formal parameter " << formalname << " is multipley defined." << endl;
    }

    Symbol declaretype = formal->getDeclareType();
    if ( m_class_symtable.lookup(declaretype) == NULL ) {
        ostream& os = semant_error(class_);
        os << "Class " << declaretype <<  " of formal parameter " << formalname << " is undefined." << endl;
    }
    SymData* symdata = new SymData(FormalType, class_, declaretype);
    symtable.addid(formalname, symdata);
    method_data->m_methodArg.push_back(formalname);
    method_data->m_methodType.push_back(declaretype);
}

void ClassTable::semant_expression(class__class* class_, Expression expr) {
    TreeType type = expr->getType();
    switch (type) {
        case AssignType:
            {
                assign_class* assign = static_cast<assign_class*>(expr);
                Symbol name = assign->getName();
                MySymTable symtable = class_->getSymTable();
                SymData* symdata = symtable.lookup(name);
                if ( symdata == NULL ) {
                    ostream& os = semant_error(class_);
                    os << "Assignment to undeclared variable " << name << "." << endl;
                }
                semant_expression(class_, assign->getExpression());
                Symbol ret_type = assign->getExpression()->type;
                //if ( ret_type != No_type && ret_type != symdata->m_type ) {
                if ( ret_type != symdata->m_type ) {
                    ostream& os = semant_error(class_);
                    os << "Type " << ret_type << " of assigned expression does not confrom to declared type " << symdata->m_type << " of identifier " << name << "." << endl;
                }
                expr->type = ret_type;
            }
            break;
        case StaticDispatchType:
            {
                static_dispatch_class* dispatch = static_cast<static_dispatch_class*>(expr);
                Expression static_expr = dispatch->getExpression();
                semant_expression(class_, static_expr);
                Symbol class_type = static_expr->type;

                SymData* class_symdata = m_class_symtable.lookup(class_type);
                if ( class_symdata == NULL ) {
                    ostream& os = semant_error(class_);
                    // TODO
                }

                Symbol dispatch_type = dispatch->getTypeName();
                SymData* parent_symdata = m_class_symtable.lookup(dispatch_type);
                if ( parent_symdata == NULL ) {
                    ostream& os = semant_error(class_);
                    os << "Static dispatch to undefined class " << dispatch_type << endl;
                }
                else {
                    if ( class_symdata != NULL ) {
                        for ( class__class* now_class = class_symdata->m_class; ; ) {
                            Symbol parent = now_class->getParent();
                            if ( parent == No_class ) {
                                ostream& os = semant_error(class_);
                                os << "Expression type " << class_type << " does not conform to declared static dispatch type " << dispatch_type << "." << endl;
                                break;
                            }
                            else if ( parent == dispatch_type ) {
                                break;
                            }
                            else {
                                now_class = m_class_symtable.lookup(parent)->m_class;
                            }
                        }
                    }
                }

                Symbol name = dispatch->getName();
                MySymTable symtable = parent_symdata->m_class->getSymTable();
                SymData* symdata = symtable.lookup(name);
                if ( symdata == NULL ) {
                    ostream& os = semant_error(class_);
                    // TODO: error message
                }

                semant_dispatch(class_, dispatch->getActual(), symdata, name, expr);
            }
            break;
        case DispatchType:
            {
                dispatch_class* dispatch = static_cast<dispatch_class*>(expr);
                Expression dispatch_expr = dispatch->getExpression();
                semant_expression(class_, dispatch_expr);
                Symbol name = dispatch->getName();

                SymData* class_symdata = m_class_symtable.lookup(dispatch_expr->type);
                MySymTable symtable = class_symdata->m_class->getSymTable();
                SymData* symdata = symtable.lookup(name);
                if ( symdata == NULL ) {
                    for ( class__class* now_class = class_ ; ; ) {
                        symtable = now_class->getSymTable();
                        SymData* symdata = symtable.lookup(name);
                        if ( symdata != NULL ) {
                            break;
                        }
                        else {
                            if ( now_class->getParent() != No_class ) {
                                SymData* parent = m_class_symtable.lookup(now_class->getParent());
                                now_class = parent->m_class;
                            }
                            else {
                                ostream& os = semant_error(class_);
                                os << "Dispatch to undefined method " << name << "." << endl;
                                break;
                            }
                        }
                    }
                }

                semant_dispatch(class_, dispatch->getActual(), symdata, name, expr);
            }
            break;
        case CondType:
            {
                cond_class* cond = static_cast<cond_class*>(expr);
                Expression pred = cond->getPred();
                semant_expression(class_, pred);
                //if ( pred->type != No_type && pred->type != Bool ) {
                if ( pred->type != Bool ) {
                    ostream& os = semant_error(class_);
                    os << "Predicate of 'if ' does not have type Bool." << endl;
                }

                Expression then_expr = cond->getThen();
                semant_expression(class_, then_expr);
                Symbol then_type = then_expr->type;

                Expression else_expr = cond->getElse();
                semant_expression(class_, else_expr);
                Symbol else_type = else_expr->type;

                // FIXME: find least upper bound
                expr->type = Object;
            }
            break;
        case LoopType:
            {
                loop_class* loop = static_cast<loop_class*>(expr);
                Expression pred = loop->getPred();
                semant_expression(class_, pred);
                //if ( pred->type != No_type && pred->type != Bool ) {
                if ( pred->type != Bool ) {
                    ostream& os = semant_error(class_);
                    os << "Loop condition does not have type Bool." << endl;
                }

                Expression body = loop->getBody();
                semant_expression(class_, body);
                expr->type = body->type;
            }
            break;
        case CaseType:
            {
                typcase_class* typcase = static_cast<typcase_class*>(expr);
                Expression expr = typcase->getExpression();
                semant_expression(class_, expr);
                Symbol case_type = expr->type;

                MySymTable symtable = class_->getSymTable();
                symtable.enterscope();

                Cases cases = typcase->getCases();
                for ( int i = cases->first(); cases->more(i); i = cases->next(i) ) {
                    semant_branch(class_, static_cast<branch_class*>(cases->nth(i)), case_type);
                }
                symtable.exitscope();
            }
            break;
        case BlockType:
            {
                Expressions exprs = static_cast<block_class*>(expr)->getExpressions();
                for ( int i = exprs->first(); exprs->more(i); i = exprs->next(i) ) {
                    semant_expression(class_, exprs->nth(i));
                    expr->type = exprs->nth(i)->type;
                }
            }
            break;
        case LetType:
            {
                let_class* let = static_cast<let_class*>(expr);
                Symbol identifier = let->getIdentifier();
                Symbol declaretype = let->getDeclareType();
                if ( m_class_symtable.lookup(declaretype) == NULL ) {
                    ostream& os = semant_error(class_);
                    os << "Class " << declaretype << " of let-bound identifier " << identifier << " is undefined." << endl;
                    expr->type = No_type;
                }

                Expression init = let->getInit();
                semant_expression(class_, init);
                //if ( init->type != No_type && init->type != declaretype ) {
                if ( init->type != declaretype ) {
                    ostream& os = semant_error(class_);
                    os << "Inferred type " << init->type << " of initialization of " << identifier << " does not confrom to identifier's declared type " << declaretype << "." << endl;
                    expr->type = No_type;
                }

                MySymTable symtable = class_->getSymTable();
                symtable.enterscope();
                SymData* symdata = new SymData(LetType, class_, declaretype);
                symtable.addid(identifier, symdata);
                Expression body = let->getBody();
                semant_expression(class_, body);
                symtable.exitscope();

                expr->type = declaretype;
            }
            break;
        case PlusType:
            {
                plus_class* plus = static_cast<plus_class*>(expr);
                Expression expr1 = plus->getExpression1();
                semant_expression(class_, expr1);
                Expression expr2 = plus->getExpression2();
                semant_expression(class_, expr2);
                if ( expr1->type != Int || expr2->type != Int || expr1->type != expr2->type ) {
                    ostream& os = semant_error(class_);
                    os << "non-Int arguments: " << expr1->type << " + " << expr2->type << endl;
                }
                expr->type = Int;
            }
            break;
        case SubType:
            {
                sub_class* sub = static_cast<sub_class*>(expr);
                Expression expr1 = sub->getExpression1();
                semant_expression(class_, expr1);
                Expression expr2 = sub->getExpression2();
                semant_expression(class_, expr2);
                if ( expr1->type != Int || expr2->type != Int || expr1->type != expr2->type ) {
                    ostream& os = semant_error(class_);
                    os << "non-Int arguments: " << expr1->type << " - " << expr2->type << endl;
                }
                expr->type = Int;
            }
            break;
        case MulType:
            {
                mul_class* mul = static_cast<mul_class*>(expr);
                Expression expr1 = mul->getExpression1();
                semant_expression(class_, expr1);
                Expression expr2 = mul->getExpression2();
                semant_expression(class_, expr2);
                if ( expr1->type != Int || expr2->type != Int || expr1->type != expr2->type ) {
                    ostream& os = semant_error(class_);
                    os << "non-Int arguments: " << expr1->type << " * " << expr2->type << endl;
                }
                expr->type = Int;
            }
            break;
        case DivideType:
            {
                divide_class* divide = static_cast<divide_class*>(expr);
                Expression expr1 = divide->getExpression1();
                semant_expression(class_, expr1);
                Expression expr2 = divide->getExpression2();
                semant_expression(class_, expr2);
                if ( expr1->type != Int || expr2->type != Int || expr1->type != expr2->type ) {
                    ostream& os = semant_error(class_);
                    os << "non-Int arguments: " << expr1->type << " / " << expr2->type << endl;
                }
                expr->type = Int;
            }
            break;
        case NegType:
            {
                neg_class* neg = static_cast<neg_class*>(expr);
                Expression negexpr = neg->getExpression();
                semant_expression(class_, negexpr);
                if ( negexpr->type != Int ) {
                    ostream& os = semant_error(class_);
                    os << "Argument of '~' has type " << negexpr->type << " instead of Int." << endl;
                }
                expr->type = Int;
            }
            break;
        case LtType:
            {
                lt_class* lt = static_cast<lt_class*>(expr);
                Expression expr1 = lt->getExpression1();
                semant_expression(class_, expr1);
                Expression expr2 = lt->getExpression2();
                semant_expression(class_, expr2);
                if ( expr1->type != expr2->type ) {
                    ostream& os = semant_error(class_);
                    os << "non-Int arguments: " << expr1->type << " < " << expr2->type << endl;
                }
                expr->type = Bool;
            }
            break;
        case EqType:
            {
                eq_class* eq = static_cast<eq_class*>(expr);
                Expression expr1 = eq->getExpression1();
                semant_expression(class_, expr1);
                Expression expr2 = eq->getExpression2();
                semant_expression(class_, expr2);
                if ( expr1->type != expr2->type ) {
                    ostream& os = semant_error(class_);
                    os << "non-Int arguments: " << expr1->type << " = " << expr2->type << endl;
                }
                expr->type = Bool;
            }
            break;
        case LeqType:
            {
                leq_class* leq = static_cast<leq_class*>(expr);
                Expression expr1 = leq->getExpression1();
                semant_expression(class_, expr1);
                Expression expr2 = leq->getExpression2();
                semant_expression(class_, expr2);
                if ( expr1->type != expr2->type ) {
                    ostream& os = semant_error(class_);
                    os << "non-Int arguments: " << expr1->type << " <= " << expr2->type << endl;
                }
                expr->type = Bool;
            }
            break;
        case CompType:
            {
                comp_class* comp = static_cast<comp_class*>(expr);
                Expression compexpr = comp->getExpression();
                semant_expression(class_, compexpr);
                if ( expr->type != Bool ) {
                    ostream& os = semant_error(class_);
                    os << "Argument of 'not' has type " << expr->type << " instead of Bool." << endl;
                }
                expr->type = Bool;
            }
            break;
        case IntType:
            expr->type = Int;
            break;
        case BoolType:
            expr->type = Bool;
            break;
        case StringType:
            expr->type = Str;
            break;
        case NewType:
            {
                new__class* newclass = static_cast<new__class*>(expr);
                Symbol name = newclass->getTypeName();
                SymData* symdata = m_class_symtable.lookup(name);
                if ( symdata == NULL ) {
                    ostream& os = semant_error(class_);
                    os << "'new' used with undefined class " << name << "." << endl;
                    expr->type = No_type;
                }
                else {
                    expr->type = symdata->m_type;
                }
            }
            break;
        case IsVoidType:
            {
                isvoid_class* isvoid = static_cast<isvoid_class*>(expr);
                Expression expr = isvoid->getExpression();
                semant_expression(class_, expr);
                expr->type = Bool;
            }
            break;
        case ObjectType:
            {
                object_class* object = static_cast<object_class*>(expr);
                Symbol name = object->getSymbol();
                MySymTable symtable = class_->getSymTable();
                if ( name == self ) {
                    expr->type = class_->getName();
                }
                else {
                    SymData* symdata = symtable.lookup(name);
                    if ( symdata == NULL ) {
                        ostream& os = semant_error(class_);
                        os << "Undeclared identifier " << name << "." << endl;
                        expr->type = No_type;
                    }
                    expr->type = symdata->m_type;
                }
            }
            break;
        default:
            //expr->dump(cout, 0);
            //cout << "type: " << type << endl;
            break;
        }
}

void ClassTable::semant_dispatch(class__class* class_, Expressions actual, SymData* symdata, Symbol name, Expression expr) {
    if ( symdata != NULL && actual->len() != static_cast<int>(symdata->m_methodType.size()) ) {
        ostream& os = semant_error(class_);
        os << "Method " << name << " called with wrong number of arguments." << endl;
        expr->type = No_type;
    }
    else {
        for ( int i = actual->first(); actual->more(i) ; i = actual->next(i) ) {
            semant_expression(class_, actual->nth(i));
            if ( symdata != NULL && actual->nth(i)->type != symdata->m_methodType[i] ) {
                ostream& os = semant_error(class_);
                os << "In call of method " << name << ", type " << actual->nth(i)->type << " of parameter " << symdata->m_methodArg[i] << " does not conform to declared type " << symdata->m_methodType[i] << "." << endl;
                expr->type = No_type;
                break;
            }
            else {
                expr->type = actual->nth(i)->type;
            }
        }
    }
}

void ClassTable::semant_branch(class__class* class_, branch_class* branch, Symbol case_type) {
    MySymTable symtable = class_->getSymTable();
    Symbol branch_name = branch->getName();
    if ( symtable.probe(branch_name) != NULL ) {
        ostream& os = semant_error(class_);
        //
    }
    else {
        Symbol branch_type = branch->getDeclareType();
        SymData* class_symdata = m_class_symtable.lookup(branch_type);
        if ( class_symdata == NULL ) {
            ostream& os = semant_error(class_);
            //
        }
        else {
            for ( class__class* now_class = class_symdata->m_class; ; ) {
                Symbol parent = now_class->getParent();
                if ( parent == No_class ) {
                    branch_type = No_type;
                    ostream& os = semant_error(class_);
                    //
                    break;
                }
                else if ( parent == case_type ) {
                    break;
                }
                else {
                    now_class = m_class_symtable.lookup(parent)->m_class;
                }
            }

            SymData* symdata = new SymData(BranchType, class_, branch_type);
            symtable.addid(branch_name, symdata);

            Expression branch_expr = branch->getExpression();
            semant_expression(class_, branch_expr);
            SymData* branch_symdata = m_class_symtable.lookup(branch_expr->type);
            if ( branch_symdata == NULL ) {
                ostream& os = semant_error(class_);
                //
            }
            for ( class__class* now_class = branch_symdata->m_class; ; ) {
                Symbol parent = now_class->getParent();
                if ( parent == No_class ) {
                    ostream& os = semant_error();
                    //
                    break;
                }
                else if ( branch_type == No_type || parent == branch_type ) {
                    break;
                }
                else {
                    now_class = m_class_symtable.lookup(parent)->m_class;
                }
            }
        }
    }
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    SymData* symdata = new SymData(ClassType, static_cast<class__class*>(Object_class), Object);
    m_class_symtable.addid(Object, symdata);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    symdata = new SymData(ClassType, static_cast<class__class*>(IO_class), IO);
    m_class_symtable.addid(IO, symdata);

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    symdata = new SymData(ClassType, static_cast<class__class*>(Int_class), Int);
    m_class_symtable.addid(Int, symdata);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    symdata = new SymData(ClassType, static_cast<class__class*>(Bool_class), Bool);
    m_class_symtable.addid(Bool, symdata);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);

    symdata = new SymData(ClassType, static_cast<class__class*>(Str_class), Str);
    m_class_symtable.addid(Str, symdata);

    // No_type
    Class_ No_type_class =
	class_(No_type, No_class, nil_Features(),filename);
    symdata = new SymData(ClassType, static_cast<class__class*>(No_type_class), No_type);
    m_class_symtable.addid(No_type, symdata);

    // prim_slot
    symdata = new SymData(ClassType, NULL, NULL);
    m_class_symtable.addid(prim_slot, symdata);

    // FIXME: self type
    symdata = new SymData(ClassType, NULL, NULL);
    m_class_symtable.addid(SELF_TYPE, symdata);

    // start to semant class content after all class names are defined
    semant_class(static_cast<class__class*>(Object_class));
    semant_class(static_cast<class__class*>(IO_class));
    semant_class(static_cast<class__class*>(Int_class));
    semant_class(static_cast<class__class*>(Bool_class));
    semant_class(static_cast<class__class*>(Str_class));
}


////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 



/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}


