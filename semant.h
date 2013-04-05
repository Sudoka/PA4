#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

class SymData {
public:
    TreeType m_type;
    Symbol m_classname;
    Symbol m_methodname;

    SymData(TreeType type, Symbol classname, Symbol methodname)
    :m_type(type), m_classname(classname), m_methodname(methodname)
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
  SymbolTable<Symbol, SymData> m_symtable;

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);

  void semant_class(class__class* class_);
  void semant_feature(class__class* class_, Feature feature);
};


#endif

