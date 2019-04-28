int main(int argc, char** argv) {
    char** foo;
    char* bar;
    if (true) {
        bar = *foo;
    } else {
        *bar = 't';
    }
    char* t = bar;
}