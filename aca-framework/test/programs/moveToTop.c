extern void __VERIFIER_error() __attribute__ ((__noreturn__));
extern void __VERIFIER_assume(int);
extern int __VERIFIER_nondet_int();

/* To be run with cpaSeq16 config */

int main () {
  int x = __VERIFIER_nondet_int();

  if (x < 0) {
    __VERIFIER_error();
  } else {
    /* FP arithmetic forces analysis to Top */
    if (x * x > 23) {
      __VERIFIER_error();
    }
  }
}
