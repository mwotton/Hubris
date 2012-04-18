#include <stdio.h>
#include <stdlib.h>
#include <ruby.h>

int loaded = 0;
VALUE Exports = Qnil;
extern void hs_init(int * argc, char ** argv[]);
void safe_hs_init() {
  char ** argv = malloc(sizeof(char**) * 1);
  int argc = 1;

  argv[0]="haskell_extension";
  if (! loaded) {
    loaded=1;
    // printf("really loading haskell runtime\n");
    hs_init(&argc, &argv);
  }
}

void Init_stub() {
  // don't do anything, we just want to make
  // sure that the other objects can see the
  // safe_hs_init symbols at the C level

  // so, ok. we do do some stuff here.:) but it's really two separate things.
  VALUE Hubris = rb_define_module("Hubris");
  Exports = rb_define_module_under(Hubris, "Exports");

}
