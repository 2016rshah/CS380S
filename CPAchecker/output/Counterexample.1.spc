CONTROL AUTOMATON ErrorPath1

INITIAL STATE ARG0;

STATE USEFIRST ARG0 :
    MATCH "" -> GOTO ARG12;
    TRUE -> STOP;

STATE USEFIRST ARG12 :
    MATCH "extern void __assert_fail (const char *__assertion, const char *__file,\n      unsigned int __line, const char *__function)\n     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));" -> GOTO ARG13_1_1;
STATE USEFIRST ARG13_0_1 :
    MATCH "extern void __assert_fail (const char *__assertion, const char *__file,\n      unsigned int __line, const char *__function)\n     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));" -> GOTO ARG13_1_1;
STATE USEFIRST ARG13_1_1 :
    MATCH "extern void __assert_perror_fail (int __errnum, const char *__file,\n      unsigned int __line, const char *__function)\n     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));" -> GOTO ARG13_2_1;
STATE USEFIRST ARG13_2_1 :
    MATCH "extern void __assert (const char *__assertion, const char *__file, int __line)\n     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));" -> GOTO ARG13_3_1;
STATE USEFIRST ARG13_3_1 :
    MATCH "extern int p1();" -> GOTO ARG13_4_1;
STATE USEFIRST ARG13_4_1 :
    MATCH "extern int p2();" -> GOTO ARG13_5_1;
STATE USEFIRST ARG13_5_1 :
    MATCH "int A1 = 101;" -> GOTO ARG13_6_1;
STATE USEFIRST ARG13_6_1 :
    MATCH "int main()" -> GOTO ARG13_7_1;
STATE USEFIRST ARG13_7_1 :
    MATCH "" -> GOTO ARG13_8_1;
STATE USEFIRST ARG13_8_1 :
    MATCH "int rng1;" -> GOTO ARG13_9_1;
STATE USEFIRST ARG13_9_1 :
    MATCH "int rng2;" -> GOTO ARG13_10_1;
STATE USEFIRST ARG13_10_1 :
    MATCH "p1()" -> GOTO ARG13_11_1;
STATE USEFIRST ARG13_11_1 :
    MATCH "p1()" -> ASSUME {__CPAchecker_TMP_0 == (0);rng2 == (102);A1 == (101);} GOTO ARG13;
    TRUE -> STOP;

STATE USEFIRST ARG13 :
    MATCH "[__CPAchecker_TMP_0 == 0]" -> GOTO ARG14;
    TRUE -> STOP;

STATE USEFIRST ARG14 :
    MATCH "int seed1 = A1;" -> GOTO ARG18_1_2;
STATE USEFIRST ARG18_0_2 :
    MATCH "int seed1 = A1;" -> GOTO ARG18_1_2;
STATE USEFIRST ARG18_1_2 :
    MATCH "int seed2 = A1;" -> GOTO ARG18_2_2;
STATE USEFIRST ARG18_2_2 :
    MATCH "((\n# 20 \"bench1/simplified-product-incorrect.c\"\n   seed1 == seed2\n# 20 \"bench1/simplified-product-incorrect.c\" 3 4\n   ) ? (void) (0) : __assert_fail (\n# 20 \"bench1/simplified-product-incorrect.c\"\n   \"seed1 == seed2\"\n# 20 \"bench1/simplified-product-incorrect.c\" 3 4\n   , \"bench1/simplified-product-incorrect.c\", 20, __PRETTY_FUNCTION__))\n# 20 \"bench1/simplified-product-incorrect.c\"\n                         ;" -> ASSUME {seed2 == (101);seed1 == (101);} GOTO ARG18;
    TRUE -> STOP;

STATE USEFIRST ARG18 :
    MATCH "[seed1 == seed2]" -> GOTO ARG19;
    TRUE -> STOP;

STATE USEFIRST ARG19 :
    MATCH "(void) (0)" -> GOTO ARG22;
    TRUE -> STOP;

STATE USEFIRST ARG22 :
    MATCH "((\n# 20 \"bench1/simplified-product-incorrect.c\"\n   seed1 == seed2\n# 20 \"bench1/simplified-product-incorrect.c\" 3 4\n   ) ? (void) (0) : __assert_fail (\n# 20 \"bench1/simplified-product-incorrect.c\"\n   \"seed1 == seed2\"\n# 20 \"bench1/simplified-product-incorrect.c\" 3 4\n   , \"bench1/simplified-product-incorrect.c\", 20, __PRETTY_FUNCTION__))\n# 20 \"bench1/simplified-product-incorrect.c\"\n                         ;" -> GOTO ARG23_1_3;
STATE USEFIRST ARG23_0_3 :
    MATCH "((\n# 20 \"bench1/simplified-product-incorrect.c\"\n   seed1 == seed2\n# 20 \"bench1/simplified-product-incorrect.c\" 3 4\n   ) ? (void) (0) : __assert_fail (\n# 20 \"bench1/simplified-product-incorrect.c\"\n   \"seed1 == seed2\"\n# 20 \"bench1/simplified-product-incorrect.c\" 3 4\n   , \"bench1/simplified-product-incorrect.c\", 20, __PRETTY_FUNCTION__))\n# 20 \"bench1/simplified-product-incorrect.c\"\n                         ;" -> GOTO ARG23_1_3;
STATE USEFIRST ARG23_1_3 :
    MATCH "p2()" -> GOTO ARG23_2_3;
STATE USEFIRST ARG23_2_3 :
    MATCH "p2()" -> ASSUME {__CPAchecker_TMP_2 == (0);} GOTO ARG23;
    TRUE -> STOP;

STATE USEFIRST ARG23 :
    MATCH "[__CPAchecker_TMP_2 == 0]" -> GOTO ARG24;
    TRUE -> STOP;

STATE USEFIRST ARG24 :
    MATCH "rng1 = seed1;" -> GOTO ARG29_1_4;
STATE USEFIRST ARG29_0_4 :
    MATCH "rng1 = seed1;" -> GOTO ARG29_1_4;
STATE USEFIRST ARG29_1_4 :
    MATCH "((\n# 28 \"bench1/simplified-product-incorrect.c\"\n   rng1 == rng2\n# 28 \"bench1/simplified-product-incorrect.c\" 3 4\n   ) ? (void) (0) : __assert_fail (\n# 28 \"bench1/simplified-product-incorrect.c\"\n   \"rng1 == rng2\"\n# 28 \"bench1/simplified-product-incorrect.c\" 3 4\n   , \"bench1/simplified-product-incorrect.c\", 28, __PRETTY_FUNCTION__))\n# 28 \"bench1/simplified-product-incorrect.c\"\n                       ;" -> ASSUME {rng1 == (101);} GOTO ARG29;
    TRUE -> STOP;

STATE USEFIRST ARG29 :
    MATCH "[!(rng1 == rng2)]" -> GOTO ARG31;
    TRUE -> STOP;

STATE USEFIRST ARG31 :
    MATCH "__assert_fail (\n# 28 \"bench1/simplified-product-incorrect.c\"\n   \"rng1 == rng2\"\n# 28 \"bench1/simplified-product-incorrect.c\" 3 4\n   , \"bench1/simplified-product-incorrect.c\", 28, __PRETTY_FUNCTION__)" -> ERROR;
    TRUE -> STOP;

STATE USEFIRST ARG33 :
    TRUE -> STOP;

END AUTOMATON
