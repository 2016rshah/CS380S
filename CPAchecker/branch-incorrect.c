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
	if(x1 != x2) {
		goto ERROR;
	}
	return (0);
  ERROR:
  return (-1);
}

