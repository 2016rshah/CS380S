int main(int argc, char** argv) {
    char* t = "asdf";
    *t = "zxcv";
    char** foo = t;
    char* bar = *t;
    int b = 0;
    // char* camelCase = b ? bar : bar;
    // camelCase = b ? *foo : *foo;
    for (;;) {
        int baz;
        int zxcv = 34;
        zxcv = 3 + baz;
        zxcv &= baz + 3;
    }
    if (t == "qwer") {
        *t = "1234";
        return 1;
    } else if (t == "jklo") {
        bar = *foo;
        return 0;
    } else {
        *bar = 34;
        // **foo = "xcvvcbn";
        return 2;
    }
    for (int i = 0; i < 10; i++) {
        *t = "qwer";
    }
}

char* g() {
    return "preservational";
}

int f(int k) {
    return k;
}

extern void h();
