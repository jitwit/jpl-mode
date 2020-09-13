#include <emacs-module.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>
typedef void V;typedef intmax_t I;typedef char C;typedef V* J;
typedef ptrdiff_t DP;typedef V* (*JIT)();typedef int (*JDT)(J,C*);
typedef C* (*JGT)(J);typedef V* (*JFT)(J);typedef V* (*JSXT) (J,V*,V*,V*,V*,I);
typedef emacs_value EV; typedef emacs_env EE;typedef struct emacs_runtime ERT;
#define R return
#define LIBJ "libj.so"
#define EFUN(f) static EV f(EE*e,DP n,EV*a,V*d)
int plugin_is_GPL_compatible;
static JDT jdo;static JFT jfree;static JIT jinit;static JSXT jsmx;static JGT jgetr;
static C* estring(EE* e, EV s)
{ DP sz=0; e->copy_string_contents(e,s,NULL,&sz);
  C *es=malloc(sz); e->copy_string_contents(e,s,es,&sz); R es; }
EFUN(jeini) { R e->make_user_ptr(e,(V*)jfree,jinit()); }
EFUN(jegetr)
{ J j=e->get_user_ptr(e,a[0]); C*s=estring(e,a[1]);jdo(j,s);free(s);
  C*r=jgetr(j); R e->make_string(e,r,strlen(r)); }
int emacs_module_init (ERT* rt)
{ EE* e = rt->get_environment(rt); EV a[2]; V* lj = dlopen(LIBJ,RTLD_LAZY);
  jinit = (JIT)dlsym(lj,"JInit"); jdo = (JDT)dlsym(lj,"JDo");
  jfree = (JFT)dlsym(lj,"JFree"); jsmx = (JSXT)dlsym(lj,"JSMX");
  jgetr = (JGT)dlsym(lj,"JGetR");
  EV provide = e->intern(e,"provide"); EV fset = e->intern(e,"fset");
  a[1] = e->make_function(e,0,0,jeini,"Create a J engine",NULL);
  a[0] = e->intern(e,"j-engine"); e->funcall(e,fset,2,a);
  a[1] = e->make_function(e,2,2,jegetr,"Execute a J sentence",NULL);
  a[0] = e->intern(e,"j-getr"); e->funcall(e,fset,2,a);
  a[0] = e->intern(e, "jpl-module"); e->funcall(e,provide,1,a); R 0; }
