extern void __VERIFIER_error() __attribute__ ((__noreturn__));
extern void __VERIFIER_assume(int);
extern int __VERIFIER_nondet_int();

int globalInt;

int foo( int baz ) {
  int bar = 42;
  if (globalInt < 23) {
    __VERIFIER_error();
  }
  return bar;
}

int main () {
  int x = __VERIFIER_nondet_int();
  foo(x);  
  if (x < 0) {
    __VERIFIER_error();
  }
}
