
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"
#include <vector>
#include <set>

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
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

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

// label_idx is used for count label usage
int label_idx = 0;

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  //os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  //os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

static void emit_pop(char *reg, ostream& str)
{
  emit_load(reg,1,SP,str);
  emit_addiu(SP,SP,4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;


 /***** Add dispatch information for class String ******/

      s << "String_dispTab" << endl;                          // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 

 /***** Add dispatch information for class Int ******/

      s << "Int_dispTab" << endl;                         // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

 /***** Add dispatch information for class Bool ******/

      s << "Bool_dispTab" << endl;                          // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}

void CgenClassTable::code_classname_tables()
{
  std::vector<CgenNodeP> node_vec;
  str << "class_nameTab:" << endl;
  for (List<CgenNode> *l = nds; l; l = l->tl()) {
    node_vec.push_back(l->hd());
  }
  for ( int n = node_vec.size() - 1; n >= 0 ; --n ) {
    str << WORD;
    stringtable.lookup_string(node_vec[n]->name->get_string())->code_ref(str);
    str << endl;
  }
  str << "class_objTab:" << endl;
  for ( int n = node_vec.size() - 1; n >= 0; --n ) {
    str << WORD << node_vec[n]->name << "_protObj" << endl;
    str << WORD << node_vec[n]->name << "_init" << endl;
  }
}

void CgenClassTable::code_classdisp_tables()
{
  std::vector<CgenNodeP> node_vec;
  std::set<Symbol> node_set;
  for ( List<CgenNode> *l = nds; l; l = l->tl() ) {
    node_vec.clear();
    for ( CgenNodeP n = l->hd(); n ; n = n->get_parentnd() ) {
      node_vec.push_back(n);
    }
    for ( int i = node_vec.size() - 1; i >= 0; --i ) {
      if ( node_set.find(node_vec[i]->name) != node_set.end() ) {
        continue;
      }
      else {
        node_set.insert(node_vec[i]->name);
        code_classdisp_table(node_vec[i]);
      }
    }
  }
}

void CgenClassTable::code_classdisp_table(CgenNodeP node)
{
  if ( node->name->equal_string("_no_class", strlen("_no_class")) )
    return;

  str << node->name << "_dispTab:" << endl; 
  std::vector<CgenNodeP> node_vec;
  for ( CgenNodeP n = node; n; n = n->get_parentnd() ) {
    node_vec.push_back(n);
  }

  int index = 0;
  for ( int i = node_vec.size() - 1; i >= 0; --i ) {
    Features features = node_vec[i]->features;
    for ( int j = features->first(); features->more(j); j = features->next(j) ) {
      Feature feature = features->nth(j);
      if ( feature->get_type() == Method ) {
        str << WORD;
        str << node_vec[i]->name<< ".";
        str << static_cast<method_class*>(feature)->name << endl;
        node->set_method_index(static_cast<method_class*>(feature)->name, index++);
      }
    }
  }
}

void CgenClassTable::code_class_prototypes() {
  std::vector<CgenNodeP> node_vec;
  std::set<Symbol> node_set;
  for ( List<CgenNode> *l = nds; l; l = l->tl() ) {
    node_vec.clear();
    for ( CgenNodeP n = l->hd(); n ; n = n->get_parentnd() ) {
      node_vec.push_back(n);
    }
    for ( int i = node_vec.size() - 1; i >= 0; --i ) {
      if ( node_set.find(node_vec[i]->name) != node_set.end() ) {
        continue;
      }
      else {
        node_set.insert(node_vec[i]->name);
        code_class_prototype(node_vec[i]);
      }
    }
  }
}

void CgenClassTable::code_class_prototype(CgenNodeP node) {
  if ( node->name->equal_string("_no_class", strlen("_no_class")) )
    return;

  std::vector<CgenNodeP> node_vec;
  for ( CgenNodeP n = node; n; n = n->get_parentnd() ) {
    node_vec.push_back(n);
  }

  std::vector<attr_class*> attr_vec;
  for ( int i = node_vec.size() - 1; i >= 0; --i ) {
    Features features = node_vec[i]->features;
    for ( int j = features->first(); features->more(j); j = features->next(j) ) {
      Feature feature = features->nth(j);
      if ( feature->get_type() == Attr ) {
        attr_vec.push_back(static_cast<attr_class*>(feature));
      }
    }
  }

  // garbage collection tag
  str << WORD << "-1" << endl;
  str << node->name << "_protObj:" << endl;
  // class tag
  str << WORD << node->getClasstag() << endl;
  // object size
  str << WORD << attr_vec.size() + 3 << endl;
  // dispatch information
  str << WORD << node->name << "_dispTab" << endl;
  // attributes
  if ( node->getClasstag() == stringclasstag ) {
    IntEntry* entry = inttable.lookup_string("0");
    str << WORD;
    entry->code_ref(str);
    str << endl << WORD << 0 << endl;
  }
  else if ( node->getClasstag() == boolclasstag || node->getClasstag() == intclasstag ) {
    str << WORD << 0 << endl;
  }
  else {
    for ( int i = 0; i < attr_vec.size(); ++i ) {
      attr_class* attr = attr_vec[i];
      if ( attr->type_decl->equal_string("String", strlen("String")) ) {
        str << WORD << 0 << endl;
      }
      else {
        str << WORD << 0 << endl;
      }
    }
  }
}

CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
   stringclasstag = 4;
   intclasstag =    2;
   boolclasstag =   3;

   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();

   code();
   exitscope();
}

//
void CgenClassTable::code_objects_init() {
  std::vector<CgenNodeP> node_vec;
  std::set<Symbol> node_set;
  for ( List<CgenNode> *l = nds; l; l = l->tl() ) {
    node_vec.clear();
    for ( CgenNodeP n = l->hd(); n ; n = n->get_parentnd() ) {
      node_vec.push_back(n);
    }
    for ( int i = node_vec.size() - 1; i >= 0; --i ) {
      if ( node_set.find(node_vec[i]->name) != node_set.end() ) {
        continue;
      }
      else {
        node_set.insert(node_vec[i]->name);
        code_object_init(node_vec[i]);
      }
    }
  }
}

void CgenClassTable::code_object_init(CgenNodeP node) {
  if ( node->name->equal_string("_no_class", strlen("_no_class")) )
    return;

  str << node->name << "_init:" << endl;
  // save to stack
  emit_addiu(SP, SP, -12, str);
  emit_store(FP, 3, SP, str);
  emit_store(SELF, 2, SP, str);
  emit_store(RA, 1, SP, str);
  // frame pointer
  emit_addiu(FP, SP, 4, str);
  // save a0
  emit_move(SELF, ACC, str);
  // call parent
  Symbol parent_name = node->get_parentnd()->name;
  if ( !parent_name->equal_string("_no_class", strlen("_no_class")) ) {
    std::string buf = parent_name->get_string();
    buf += "_init";
    emit_jal((char*)buf.c_str(), str);
  }
  // load a0
  emit_move(ACC, SELF, str);
  // load from stack
  emit_load(FP, 3, SP, str);
  emit_load(SELF, 2, SP, str);
  emit_load(RA, 1, SP, str);
  emit_addiu(SP, SP, 12, str);
  // return
  emit_return(str);
}

void CgenClassTable::code_class_methods() {
  for ( List<CgenNode> *l = nds; l; l = l->tl() ) {
    if ( l->hd()->getClasstag() > stringclasstag ) {
      Features features = l->hd()->features;
      for ( int i = features->first(); features->more(i); i = features->next(i) ) {
        Feature feature = features->nth(i);
        if ( feature->get_type() == Method ) {
          method_class* method = static_cast<method_class*>(feature);
          str << l->hd()->name << "." << method->name << ":" << endl;
          // save caller's information to stack
          emit_addiu(SP, SP, -12, str);
          emit_store(FP, 3, SP, str);
          emit_store(SELF, 2, SP, str);
          emit_store(RA, 1, SP, str);

          // setup frame pointer
          emit_addiu(FP, SP, 4, str);

          // save a0
          emit_move(SELF, ACC, str);

          // generate code inside function defintion
          method->expr->code(l->hd(), str);

          // load from stack
          emit_load(FP, 3, SP, str);
          emit_load(SELF, 2, SP, str);
          emit_load(RA, 1, SP, str);
          emit_addiu(SP, SP, 12, str);

          // return
          emit_return(str);
        }
      }
    }
  }
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this,false));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this,false));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this,false));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this,true));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
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
	   filename),	    
    Basic,this,true));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this,true));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this,true));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
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
	     filename),
        Basic,this,true));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this,true));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

void CgenNode::set_method_index(Symbol name, int index)
{
  method_map.insert(std::make_pair(name, index));
}

int CgenNode::get_method_index(Symbol name)
{
  return method_map.find(name)->second;
}


void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

//                   - class_nameTab
  if (cgen_debug) cout << "class name tables" << endl;
  code_classname_tables();
//                   - dispatch tables
  if (cgen_debug) cout << "class dispatch tables" << endl;
  code_classdisp_tables();
//                   - prototype objects
  if (cgen_debug) cout << "class prototypes" << endl;
  code_class_prototypes();

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

//                   - object initializer
  if (cgen_debug) cout << "object initializer" << endl;
  code_objects_init();
//                   - the class methods
  if (cgen_debug) cout << "class methods" << endl;
  code_class_methods();
//                   - etc...
//
}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////
int CgenNode::classcount = 0;
CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct, bool inc_classtag) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
   if ( inc_classtag )
     classtag = classcount++;
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(CgenNodeP classnode, ostream &s) {
  s << "#assign" << endl;
}

void static_dispatch_class::code(CgenNodeP classnode, ostream &s) {
  s << "#static_dispatch" << endl;
  emit_bne(ACC, ZERO, label_idx, s);
  emit_load_address(ACC, "str_const0", s);
  emit_load_imm(T1, get_line_number(), s);
  emit_jal("_dispatch_abort", s);

  emit_label_def(label_idx++, s);
  // locate dispatch table
  emit_load(T1, 2, ACC, s);
  // locate dispatch function location
  int idx = classnode->get_method_index(name);
  emit_load(T1, idx, T1, s);
  // jump to method
  emit_jalr(T1, s);
}

void dispatch_class::code(CgenNodeP classnode, ostream &s) {
  s << "#dispatch" << endl;
  emit_bne(ACC, ZERO, label_idx, s);
  emit_load_address(ACC, "str_const0", s);
  emit_load_imm(T1, get_line_number(), s);
  emit_jal("_dispatch_abort", s);

  emit_label_def(label_idx++, s);
  // locate dispatch table
  emit_load(T1, 2, ACC, s);
  // locate dispatch function location
  int idx = classnode->get_method_index(name);
  emit_load(T1, idx, T1, s);
  // jump to method
  emit_jalr(T1, s);
}

void cond_class::code(CgenNodeP classnode, ostream &s) {
  s << "#cond" << endl;
  int branch_true = label_idx++;
  int branch_end = label_idx++;

  pred->code(classnode, s);
  emit_load_bool(T1, truebool, s);
  emit_beq(ACC, T1, branch_true, s);
  // false
  else_exp->code(classnode, s);
  emit_branch(branch_end, s);
  // true
  emit_label_def(branch_true, s);
  then_exp->code(classnode, s);
  // end
  emit_label_def(branch_end, s);
}

void loop_class::code(CgenNodeP classnode, ostream &s) {
  s << "#loop" << endl;
  int branch_cond = label_idx++;
  int branch_true = label_idx++;
  int branch_end = label_idx++;

  emit_label_def(branch_cond, s);
  pred->code(classnode, s);
  emit_move(T1, ACC, s);
  emit_load_bool(ACC, truebool, s);
  emit_beq(T1, ACC, branch_true, s);
  // false
  emit_branch(branch_end, s);
  // true
  emit_label_def(branch_true, s);
  body->code(classnode, s);
  emit_branch(branch_cond, s);
  // end
  emit_label_def(branch_end, s);
}

void typcase_class::code(CgenNodeP classnode, ostream &s) {
  s << "#typcase" << endl;
}

void block_class::code(CgenNodeP classnode, ostream &s) {
  for ( int i = body->first(); body->more(i); i = body->next(i) ) {
    body->nth(i)->code(classnode, s);
  }
}

void let_class::code(CgenNodeP classnode, ostream &s) {
  s << "#let" << endl;
}

void plus_class::code(CgenNodeP classnode, ostream &s) {
  s << "#plus" << endl;
  e1->code(classnode, s);
  emit_push(ACC, s);
  e2->code(classnode, s);
  emit_pop(T1, s);
  emit_add(ACC, T1, ACC, s);
  emit_addiu(SP, SP, -4, s);
}

void sub_class::code(CgenNodeP classnode, ostream &s) {
  s << "#sub" << endl;
  e1->code(classnode, s);
  emit_push(ACC, s);
  e2->code(classnode, s);
  emit_pop(T1, s);
  emit_sub(ACC, T1, ACC, s);
  emit_addiu(SP, SP, -4, s);
}

void mul_class::code(CgenNodeP classnode, ostream &s) {
  s << "#mul" << endl;
  e1->code(classnode, s);
  emit_push(ACC, s);
  e2->code(classnode, s);
  emit_pop(T1, s);
  emit_mul(ACC, T1, ACC, s);
  emit_addiu(SP, SP, -4, s);
}

void divide_class::code(CgenNodeP classnode, ostream &s) {
  s << "#div" << endl;
  e1->code(classnode, s);
  emit_push(ACC, s);
  e2->code(classnode, s);
  emit_pop(T1, s);
  emit_div(ACC, T1, ACC, s);
  emit_addiu(SP, SP, -4, s);
}

void neg_class::code(CgenNodeP classnode, ostream &s) {
  s << "#neg" << endl;
}

void lt_class::code(CgenNodeP classnode, ostream &s) {
  s << "#lt" << endl;
  int branch_true = label_idx++;
  int branch_end = label_idx++;

  e1->code(classnode, s);
  emit_push(ACC, s);
  e2->code(classnode, s);
  emit_pop(T1, s);
  emit_blt(T1, ACC, branch_true, s);
  // false
  emit_load_bool(ACC, falsebool, s);
  emit_branch(branch_end, s);
  // true
  emit_label_def(branch_true, s);
  emit_load_bool(ACC, truebool, s);
  // end
  emit_label_def(branch_end, s);
}

void eq_class::code(CgenNodeP classnode, ostream &s) {
  s << "#eq" << endl;
  int branch_true = label_idx++;
  int branch_end = label_idx++;

  e1->code(classnode, s);
  emit_push(ACC, s);
  e2->code(classnode, s);
  emit_pop(T1, s);
  emit_beq(T1, ACC, branch_true, s);

  // false
  // object comparison
  emit_move(T2, ACC, s);
  // initial a0: true, a1: false
  emit_load_bool(ACC, truebool, s);
  emit_load_bool(A1, falsebool, s);
  emit_jal("equality_test", s);
  emit_branch(branch_end, s);
  // true
  emit_label_def(branch_true, s);
  emit_load_bool(ACC, truebool, s);
  // end
  emit_label_def(branch_end, s);
}

void leq_class::code(CgenNodeP classnode, ostream &s) {
  s << "#leq" << endl;
  int branch_true = label_idx++;
  int branch_end = label_idx++;

  e1->code(classnode, s);
  emit_push(ACC, s);
  e2->code(classnode, s);
  emit_pop(T1, s);
  emit_bleq(T1, ACC, branch_true, s);
  // false
  emit_load_bool(ACC, falsebool, s);
  emit_branch(branch_end, s);
  // true
  emit_label_def(branch_true, s);
  emit_load_bool(ACC, truebool, s);
  // end
  emit_label_def(branch_end, s);
}

void comp_class::code(CgenNodeP classnode, ostream &s) {
  s << "#comp" << endl;
  int branch_true = label_idx++;
  int branch_end = label_idx++;

  e1->code(classnode, s);
  emit_move(T1, ACC, s);
  emit_load_bool(ACC, truebool, s);
  emit_beq(T1, ACC, branch_true, s);
  // false
  emit_load_bool(ACC, truebool, s);
  emit_branch(branch_end, s);
  // true
  emit_label_def(branch_true, s);
  emit_load_bool(ACC, falsebool, s);
  // end
  emit_label_def(branch_end, s);
}

void int_const_class::code(CgenNodeP classnode, ostream& s)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(CgenNodeP classnode, ostream& s)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(CgenNodeP classnode, ostream& s)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(CgenNodeP classnode, ostream &s) {
  s << "#new" << endl;
}

void isvoid_class::code(CgenNodeP classnode, ostream &s) {
  s << "#isvoid" << endl;
}

void no_expr_class::code(CgenNodeP classnode, ostream &s) {
  s << "#expr" << endl;
}

void object_class::code(CgenNodeP classnode, ostream &s) {
  s << "#obj" << endl;
}

