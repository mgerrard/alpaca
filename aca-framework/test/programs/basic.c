extern void __VERIFIER_error() __attribute__ ((__noreturn__));
extern void __VERIFIER_assume(int);
extern int __VERIFIER_nondet_int();

int main () {
  int x = __VERIFIER_nondet_int();
  int z;
  z = __VERIFIER_nondet_int();
  if (x < 0) {
    __VERIFIER_error();
  }
}
