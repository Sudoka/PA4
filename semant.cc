

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

/* create symbol table */

ClassTable::ClassTable(Classes classes)
: semant_errors(0) , error_stream(cerr) {
    m_symtable.enterscope();

    /* Fill this in */
    install_basic_classes();

    for ( int i = classes->first(); classes->more(i); i = classes->next(i) ) {
        class__class* class_ = static_cast<class__class*>(classes->nth(i));
        if ( m_symtable.probe(class_->getName()) != NULL ) {
            ostream& os = semant_error(class_);
            os << "Class" << class_->getName() << " was previously defined." << endl;
        }
        SymData* symdata = new SymData(ClassType, class_->getName(), NULL);
        m_symtable.addid(class_->getName(), symdata);
    }

    for ( int i = classes->first(); classes->more(i); i = classes->next(i) ) {
        class__class* class_ = static_cast<class__class*>(classes->nth(i));
        semant_class(class_);
    }
    m_symtable.exitscope();
}

void ClassTable::semant_class(class__class* class_) {
    if ( m_symtable.probe(class_->getParent()) == NULL && class_->getName() != Object ) {
        ostream& os = semant_error(class_);
        os << "Class" << class_->getName() << " inherits from an undefined class " << class_->getParent() << "." << endl;
    }

    m_symtable.enterscope();
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
    m_symtable.exitscope();
}

void ClassTable::semant_attr(class__class* class_, Feature feature) {
    attr_class* attr = static_cast<attr_class*>(feature);
    Symbol attrname = attr->getName();
    if ( m_symtable.probe(attrname) != NULL ) {
        ostream& os = semant_error(class_);
        os << "Attribute " << attrname << " is multiply defined in class." << endl;
    }

    Symbol declaretype = attr->getDeclareType();
    if ( m_symtable.lookup(declaretype) == NULL ) {
        ostream& os = semant_error(class_);
        os << "Class " << declaretype << " of attribute " << attrname << " is undefined." << endl;
    }

    SymData* symdata = new SymData(AttrType, class_->getName(), NULL);
    m_symtable.addid(attr->getName(), symdata);
}

void ClassTable::semant_method(class__class* class_, Feature feature) {
    method_class* method = static_cast<method_class*>(feature);
    Symbol methodname = method->getName();
    if ( m_symtable.probe(methodname) != NULL ) {
        ostream& os = semant_error(class_);
        os << "Method" << methodname << " is multiply defined." << endl;
    }

    Symbol returntype = method->getReturnType();
    if ( m_symtable.lookup(returntype) == NULL ) {
        ostream& os = semant_error(class_);
        os << "Undefined return type " << returntype << " in method " << methodname << "." << endl;
    }

    SymData* symdata = new SymData(MethodType, class_->getName(), NULL);
    m_symtable.addid(method->getName(), symdata);

    Formals formals = method->getFormals();
    m_symtable.enterscope();
    for ( int i = formals->first(); formals->more(i); i = formals->next(i) ) {
        semant_formal(class_, static_cast<formal_class*>(formals->nth(i)));
    }

    // expr here
    Expression expr = method->getExpression();
    semant_expression(class_, expr);

    m_symtable.exitscope();
}

void ClassTable::semant_formal(class__class* class_, formal_class* formal) {
    Symbol formalname = formal->getName();
    if ( m_symtable.probe(formalname) != NULL ) {
        ostream& os = semant_error(class_);
        os << "Formal parameter " << formalname << " is multipley defined." << endl;
    }
    SymData* symdata = new SymData(FormalType, class_->getName(), NULL);
    m_symtable.addid(formalname, symdata);

    Symbol declaretype = formal->getDeclareType();
    if ( m_symtable.lookup(declaretype) == NULL ) {
        ostream& os = semant_error(class_);
        os << "Class " << declaretype <<  " of formal parameter " << formalname << " is undefined." << endl;
    }
}

void ClassTable::semant_expression(class__class* class_, Expression expr) {
    TreeType type = expr->getType();
    switch (type) {
        case AssignType:
            {
                assign_class* assign = static_cast<assign_class*>(expr);
                Symbol name = assign->getName();
                if ( m_symtable.lookup(name) == NULL ) {
                    ostream& os = semant_error(class_);
                    os << "Assignment to undeclared variable " << name << "." << endl;
                }
                semant_expression(class_, assign->getExpression());
            }
            break;
        case BlockType:
            {
                Expressions exprs = static_cast<block_class*>(expr)->getExpressions();
                for ( int i = exprs->first(); exprs->more(i); i = exprs->next(i) ) {
                    semant_expression(class_, exprs->nth(i));
                }
            }
            break;
        case ObjectType:
            {
                object_class* object = static_cast<object_class*>(expr);
                Symbol name = object->getName();
                if ( name == self ) {
                }
                else if ( m_symtable.lookup(name) == NULL ) {
                    ostream& os = semant_error(class_);
                    os << "Undeclared identifier " << name << "." << endl;
                }
            }
            break;
        default:
            //expr->dump(cout, 0);
            //cout << "type: " << type << endl;
            break;
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

    SymData* symdata = new SymData(ClassType, Object, NULL);
    m_symtable.addid(Object, symdata);

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

    symdata = new SymData(ClassType, IO, NULL);
    m_symtable.addid(IO, symdata);

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    symdata = new SymData(ClassType, Int, NULL);
    m_symtable.addid(Int, symdata);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    symdata = new SymData(ClassType, Bool, NULL);
    m_symtable.addid(Bool, symdata);

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

    symdata = new SymData(ClassType, Str, NULL);
    m_symtable.addid(Str, symdata);

    // prim_slot
    symdata = new SymData(ClassType, prim_slot, NULL);
    m_symtable.addid(prim_slot, symdata);

    // FIXME: self type
    symdata = new SymData(ClassType, SELF_TYPE, NULL);
    m_symtable.addid(SELF_TYPE, symdata);

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


