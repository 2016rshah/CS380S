int main(int argc, char** argv) {
    char* t = "asdf";
    *t = "zxcv";
    char** foo = t;
    char* bar = *t;
    if (t == "qwer") {
        return 1;
    } else if (t == "jklo") {
        return 0;
    } else {
        return 2;
    }
    for (int i = 0; i < 10; i++) {
        *t = "qwer";
    }
    int k = 0;
    k = 4;
    while (k < 10) {
        k++;
        *t = "jkl;";
    }
}

void f() {
    int testVar = 3;
}