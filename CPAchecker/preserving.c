#include "assert.h"

extern int preserving(int x, int y);
extern int non_preserving(int x, int y);

int main() {
  assert(
	 preserving(preserving(1, 2), 3)
	 ==
	 preserving(non_preserving(1, 2), 3)
	 );
  return 0;
}

