void __assert_fail(const char *__assertion, const char *__file, unsigned int __line, const char *__function);
void __assert_perror_fail(int __errnum, const char *__file, unsigned int __line, const char *__function);
void __assert(const char *__assertion, const char *__file, int __line);
int p1();
int p2();
int A1 = 101;
int main();
extern void __VERIFIER_error(void);
int main_0();
int main_0() {
  int rng1;
  int rng2;
  int __CPAchecker_TMP_0;
  __CPAchecker_TMP_0 = p1();
  __CPROVER_assume(__CPAchecker_TMP_0 == 0);
  int seed1 = A1;
  int seed2 = A1;
  void __CPAchecker_TMP_1;
  __CPROVER_assume(seed1 == seed2);
  0;
  0;
  int __CPAchecker_TMP_2;
  __CPAchecker_TMP_2 = p2();
  __CPROVER_assume(__CPAchecker_TMP_2 == 0);
  rng1 = seed1;
  void __CPAchecker_TMP_3;
  __CPROVER_assume(!(rng1 == rng2));
  __VERIFIER_error(); // target state
  __assert_fail("rng1 == rng2", "bench1/simplified-product-incorrect.c", 28, "__PRETTY_FUNCTION__");
}
