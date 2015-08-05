#include<iostream>
#include<string>

// an expression simplifier. 
// This is c++14 code.. compile with clang++ -std=c++14   
// or g++ -std=c++14

using std::ostream;
using std::cout;

struct expression {
  enum class tag { VAR, CONST, ADD, MULT } type;
  union {
     const char* const name;
     int    constant;
     struct { expression* op1; expression* op2; };
  };

  expression(tag t, expression* a, expression* b) : 
     type(t), op1(a), op2(b) { }
  expression(const char *const n) : 
     type(tag::VAR), name(n) { }
  expression(int i) : type(tag::CONST), constant(i) { }

  ~expression() {
    if(type == tag::ADD || type == tag::MULT) {
      delete op1;
      delete op2;
    }
  }

};

// some helper macros to make creating expressions easier..
// could also use inline functions if desired. CONEXP covers both
// constants and variables. Maybe that's a little too clever.
#define ADDEXP(a,b) new expression(expression::tag::ADD, (a), (b))
#define MULEXP(a,b) new expression(expression::tag::MULT, (a), (b))
#define CONEXP(a) new expression(a)

// helpers for teasing out constants...
bool isConst(expression *e) { return e->type == expression::tag::CONST; }
bool isConst(expression *e, int c) {
  return (e->type == expression::tag::CONST && e->constant == c);
}

expression* simplify1(expression *e) {
  expression* ans = e;
  switch(e->type) {
    case expression::tag::ADD:
      if(isConst(e->op1,0)) { ans = new expression(*(e->op2)); } 
      else if(isConst(e->op2,0)) { ans = new expression(*(e->op1)); }
      else if(isConst(e->op1) && isConst(e->op2)) {
         ans = new expression(e->op1->constant + e->op2->constant);
      }
      break;
    case expression::tag::MULT:
      if(isConst(e->op1,0) || isConst(e->op2,0)) { 
         ans = new expression(0);
      } else if(isConst(e->op1,1)) { ans = new expression(*(e->op2)); }
      else if(isConst(e->op2,1)) { ans = new expression(*(e->op1)); }
      else if(isConst(e->op1) && isConst(e->op2)) {
         ans = new expression(e->op1->constant * e->op2->constant);
      }
      break;
    default:
      break;
  }
  if(ans != e) { delete e; }
  return ans;
}

expression* simplify(expression *e) {
  expression * arg = 0;
  if(e->type == expression::tag::ADD || e->type == expression::tag::MULT) {
      arg = new expression(e->type, simplify(e->op1), simplify(e->op2));
  } else {
     arg = new expression(*e);
  }
  return simplify1(arg);
}

ostream& operator<<(ostream &os, expression &e) {
  if(e.type == expression::tag::CONST) {
     os << e.constant;
  }
  else { os << "The expr could not be simplified to a string."; }
  return os;
}

int main() {
  auto e = ADDEXP(MULEXP(ADDEXP(CONEXP(1),
                                MULEXP(CONEXP(0),CONEXP("X"))),
                         CONEXP(3)),
                 CONEXP(12));
  auto se = simplify(e);
  cout << "The expression reduces to: " << *se << std::endl;
  delete se;
  delete e; 
  return 0;
}
