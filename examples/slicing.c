extern void __VERIFIER_error() __attribute__ ((__noreturn__));
extern int __VERIFIER_nondet_int();

int main () {
  
  int x = __VERIFIER_nondet_int();
  int y = __VERIFIER_nondet_int();
  int z = 0;

  if (y > 0) {
    z = 3;
  }
  
  if (x < 0) {
    __VERIFIER_error();
  }
  
}
