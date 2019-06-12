int main(int argc, char** argv) {
    return main1(argc, argv);
}

int main1(int test, char** test_c) {
    char** foo;
    char* bar;
    int k = 0;
    if (k == 0) {
        bar = *foo;
    } else {
        *bar = 't';
    }
    char* t = bar;
}