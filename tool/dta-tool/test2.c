int main(int argc, char** argv) {
    char* t = "asdf";
    *t = "zxcv";
    char** foo = t;
    char* bar = *t;
    for (;;) {
        int baz;
        int zxcv = 34;
    }
    if (t == "qwer") {
        *t = "1234";
        return 1;
    } else if (t == "jklo") {
        *bar = "dfgh";
        return 0;
    } else {
        **foo = "xcvvcbn";
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

char* g() {
    return "preservational";
}

void f() {
    int testVar = 3;
}