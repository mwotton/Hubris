#ifndef __FOOSHIM__
#define __FOOSHIM__ 1
#define HAVE_STRUCT_TIMESPEC 1 
#include <ruby.h>
/* this is about as filthy as it looks, but c2hs chokes otherwise. */

// did this really have to be a macro? BAD MATZ
unsigned int rtype(VALUE obj);
VALUE int2fix(int i);
int fix2int(VALUE x);
double num2dbl(VALUE x);
unsigned int rb_ary_len(VALUE x);

/* enum StaticValue { */
/*   QNIL = Qnil, */
/*   QFALSE = Qfalse, */
/*   QTRUE = Qtrue, */
/* }; */




// argh, and again
enum RubyType {
 RT_NONE     = T_NONE,

 RT_NIL      = T_NIL    ,
 RT_OBJECT   = T_OBJECT ,
 RT_CLASS    = T_CLASS  ,
 RT_ICLASS   = T_ICLASS ,
 RT_MODULE   = T_MODULE ,
 RT_FLOAT    = T_FLOAT  ,
 RT_STRING   = T_STRING ,
 RT_REGEXP   = T_REGEXP ,
 RT_ARRAY    = T_ARRAY  ,
 RT_FIXNUM   = T_FIXNUM ,
 RT_HASH     = T_HASH   ,
 RT_STRUCT   = T_STRUCT ,
 RT_BIGNUM   = T_BIGNUM ,
 RT_FILE     = T_FILE   ,

 RT_TRUE     = T_TRUE   ,
 RT_FALSE    = T_FALSE  ,
 RT_DATA     = T_DATA   ,
 RT_MATCH    = T_MATCH  ,
 RT_SYMBOL   = T_SYMBOL ,

 // t_BLKTAG   = T_BLKTAG , // this one is broken in ruby 1.9

 RT_UNDEF    = T_UNDEF  ,
 // t_VARMAP   = T_VARMAP , // this one is broken in ruby 1.9
 // t_SCOPE    = T_SCOPE  , // this one is broken in ruby 1.9
 RT_NODE     = T_NODE   ,

 RT_MASK     = T_MASK   ,
};
#endif


