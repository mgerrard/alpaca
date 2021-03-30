extern void __VERIFIER_error() __attribute__ ((__noreturn__));
extern void __VERIFIER_assume(int);
extern int __VERIFIER_nondet_int();

/* Running only with CPA's -sv-comp16 config */

int main( ) {
  int x = __VERIFIER_nondet_int();
  int y = __VERIFIER_nondet_int();

  if (x < 0) {
    if ((x*(y+1)) < 23) {
      __VERIFIER_error();
    }
  }
}
