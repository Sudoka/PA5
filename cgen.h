#include <assert.h>
#include <stdio.h>
#include <vector>
#include <map>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;


// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();
   void code_classname_tables();
   void code_classdisp_tables();
   void code_classdisp_table(CgenNodeP node);
   void code_class_prototypes();
   void code_class_prototype(CgenNodeP node);
   void code_objects_init();
   void code_object_init(CgenNodeP node);
   void code_class_methods();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
};


class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   int classtag;                              // classtag
   static int classcount;                     // classcount for classtag
   std::map<Symbol, int> attr_map;            // attr <-> idx map
   std::vector<std::string> method_map;       // method <-> idx map

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table,
            bool inc_classtag);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
   int get_classtag() { return classtag; }
   void set_attr_index(Symbol name, int index);
   int get_attr_index(Symbol name);
   void set_method_index(Symbol name, int index);
   int get_method_index(Symbol name);
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

