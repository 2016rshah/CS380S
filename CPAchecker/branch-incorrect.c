#include "assert.h"

extern int f();

int main() {
	int x1;
	int x2;
	if(f()) {
		x1 = 1;
		x2 = 2;	
	} 
	else {
		x1 = 2;
		x2 = 1;
	}
	assert(x1 == x2);
	return 0;
}

