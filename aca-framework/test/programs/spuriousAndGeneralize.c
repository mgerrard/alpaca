extern void __VERIFIER_error() __attribute__ ((__noreturn__));
extern void __VERIFIER_assume(int);
extern int __VERIFIER_nondet_int();

int main () {
  int y = __VERIFIER_nondet_int();
  int tmp = 0;

  if (y >= 0) {
    for (int i=0; i<y; i++) {
      tmp += i;
    }
  } else {
    tmp = y * y;
  }

  if (tmp <= 0) {
    __VERIFIER_error();
  }
}
