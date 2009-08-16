#include "rshim.h"
#include <stdio.h>

void Init_rshim() {
  printf("loaded, bitches\n");
}

// did this really have to be a macro? BAD MATZ
unsigned int rtype(VALUE obj) {
  return TYPE(obj);
}
