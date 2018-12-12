#include "assert.h"

extern int f();
extern int g1();
extern int g2();
extern int taint();

int main() {
  int x1;
  int x2;
  int y1;
  int y2;
  
  if(f()) {
    x1 = g1();
    if(f()) {
      x2 = g1();
    }
    else {
      x2 = g2();
    }
    if(x2 == 7) {
      y2 = taint();
    }
  }
  else {
    x1 = g2();
    if(f()) {
      x2 = g1();
    }
    else {
      x2 = g2();
    }
    if(x2 == 7) {
      y2 = taint();
    }
  }
  
  if(x1 == 7) {
    y1 = taint();
  }
  
  assert(y1 == y2);
  return (0);
}
