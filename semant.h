#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"
#include <vector>

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

class SymData {
public:
    TreeType m_treetype;
    class__class* m_class;
    Symbol m_type;
    std::vector<Symbol> m_methodArg;
    std::vector<Symbol> m_methodType;

    SymData(TreeType treetype, class__class* class_, Symbol type)
    :m_treetype(treetype), m_class(class_), m_type(type)
    {}
};

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;
  SymbolTable<Symbol, SymData> m_class_symtable;

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);

  void semant_class(class__class* class_);
  void semant_class_expr(class__class* class_);
  void semant_attr(class__class* class_, Feature feature);
  void semant_method(class__class* class_, Feature feature);
  void semant_method_expr(class__class* class_, Feature feature);
  void semant_formal(class__class* class_, SymData* method_data, formal_class* formal);
  void semant_expression(class__class* class_, Expression expr);
  void semant_dispatch(class__class* class_, Expressions actual, SymData* symdata, Symbol name);
  Symbol semant_branch(class__class* class_, branch_class* branch, Symbol case_type);

  bool check_parent_type(class__class* class_, Symbol now_type, Symbol correct_type);
  SymData* get_symdata(class__class* class_, Symbol child);
  Symbol get_least_upper_bound(class__class* class_, Symbol type1, Symbol type2);
};


#endif

