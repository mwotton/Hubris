
#include "rshim.h"

#include <ruby.h>
#include <stdio.h>

/* void Init_rshim() { */
/*   printf("loaded, bitches\n"); */
/* } */

// did this really have to be a macro? BAD MATZ
unsigned int rtype(VALUE obj) {
  return TYPE(obj);
}

VALUE int2fix(int x) {
  return INT2FIX(x);
}

int fix2int(VALUE x) {
  return FIX2INT(x);
}

double num2dbl(VALUE x) {
  return NUM2DBL(x);
}

unsigned int rb_ary_len(VALUE x) {
  return RARRAY_LEN(x);
}

VALUE keys(VALUE hash) {
  rb_funcall(hash, rb_intern("keys"), 0);
}
