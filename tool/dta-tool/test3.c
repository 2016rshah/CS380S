int main(int argc, char** argv) {
    char** foo;
    char* bar;
    if (true) {
        bar = *foo;
    } else if (false) {
        *bar = 't';
    } else {
        2+2;
    }
    char* t = bar;
}