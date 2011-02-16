
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

VALUE int2fix(long x) {
  // printf("long2fix called\n");
  return LONG2FIX(x);
}

long fix2int(VALUE x) {
  // printf("fix2long called\n");
  // return rb_num2int(x);
  return FIX2LONG(x);
  //return FIX2INT(x);
}

double num2dbl(VALUE x) {
  //  printf("num2dbl called\n");
  return NUM2DBL(x);
}

unsigned int rb_ary_len(VALUE x) {
  return RARRAY_LEN(x);
}

VALUE keys(VALUE hash) {
  rb_funcall(hash, rb_intern("keys"), 0);
}

VALUE buildException(char * message) {
  VALUE errclass = rb_eval_string("HaskellError");
  VALUE errobj = rb_exc_new2(errclass, message);
  return errobj;
}

