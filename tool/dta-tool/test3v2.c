int main(int argc, char** argv) {
    char** foo;
    char* bar;
    int k = 0;
    if (k == 1) {
        bar = *foo;
    } else {
        *bar = 't';
    }
    char* t = bar;
}