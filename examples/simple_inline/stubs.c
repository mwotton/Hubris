/* so, here's the story. We have the functions, and we need to expose them to Ruby */
#include "rshim.h"
VALUE Target = Qnil;
extern void hs_init(int * argc, char ** argv[]);

void Init_libmytriple_6f568dbbfd5c587c005798fabfbb0873() {
    int argc = 1;
    // this needs to be allocated on the heap or we get a segfault
    char ** argv = malloc(sizeof(char**) * 1);
    argv[0]="haskell_extension";
    hs_init(&argc, &argv);
    Target = rb_define_class("Target", rb_cObject);
VALUE mytriple_external(VALUE);
rb_define_method(Target,"mytriple",mytriple_external, 1);
}
