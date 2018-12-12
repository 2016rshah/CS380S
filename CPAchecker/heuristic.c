#include "assert.h"

extern int f();
extern int g1();
extern int g2();
extern int taint();
extern int getPID();

int main() {
  int x1;
  int x2;
  int y1;
  int y2;
  
  if(f()) {
    x1 = g1();
  }
  else {
    x1 = g2();
  }
  if(x1 == 7) {
    y1 = taint();
  } else {
    y1 = getPID();
  }

  if(f()) {
    x2 = g1();
  }
  else {
    x2 = g2();
  }
  if(x2 == 7) {
    y2 = taint();
  } else {
    y2 = getPID();
  }
  
  assert(y1 == y2);
  return (0);
}
