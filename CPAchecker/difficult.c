#include "assert.h"

extern int f();
extern int g();

int main() {
  int x1;
  int x2;

  if(f()) {
    x1 = 1;
    if(g()) {
      x2 = 1;
    }
    else {
      x2 = 2;
    }
  }
  else {
    x1 = 2;
    if(g()) {
      x2 = 1;
    }
    else {
      x2 = 2;
    }
  }
    
  assert(x1 == x2);
  return (0);
}
