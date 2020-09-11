#include <emacs-module.h>
typedef void V;typedef intmax_t I;typedef char C;typedef V* J;
typedef ptrdiff_t DP;typedef V* (*JIT)();typedef int (*JDT)(J,C*);
typedef V* (*JFT)(J);typedef V* (*JSXT) (J,V*,V*,V*,V*,I);
typedef emacs_value EV; typedef emacs_env EE;typedef struct emacs_runtime ERT;
#include <stdlib.h>
#include <stdio.h>
#include <dlfcn.h>
#define R return
#define LIBJ "libj.so"
#define MTYOEXIT 5
#define EFUN(f) static EV f(EE*e,DP n,EV*a,V*d)
int plugin_is_GPL_compatible;
static JDT jdo;static JFT jfree;static JIT jinit;static JSXT jsmx;
static V jputs (J j, int t, C* s) // make me not exit *emacs*
{ if(MTYOEXIT==t) exit((int)(I)s); fputs(s,stdout); fflush(stdout); }
static C* estring(EE* e, EV s)
{ DP sz = 0; e->copy_string_contents(e,s,NULL,&sz);
  C *es = malloc(sz); e->copy_string_contents(e,s,es,&sz); R es; }
EFUN(jedo)
{ J j = e->get_user_ptr(e,a[0]); C* s = estring(e,a[1]);
  int r = jdo(j,s); free(s); R e->make_integer(e,r); }
// need to respond to C-g
// static void sigint(int k){**adadbreak+=1;signal(SIGINT,sigint);}
// adadbreak=(char**)jt; // first address in jt is address of breakdata
EFUN(jeini) { R e->make_user_ptr(e,(V*)jfree,jinit()); }
EFUN(jesmx)
{ J j=e->get_user_ptr(e,a[0]);C*o=estring(e,a[1]);freopen(o,"a+",stdout);free(o);
  jsmx(j,jputs,NULL,NULL,NULL,8); R e->make_integer(e,0); }
int emacs_module_init (ERT* rt)
{ EE* e = rt->get_environment(rt); EV a[2]; V* lj = dlopen(LIBJ,RTLD_LAZY);
  jinit = (JIT)dlsym(lj,"JInit"); jdo = (JDT)dlsym(lj,"JDo");
  jfree = (JFT)dlsym(lj,"JFree"); jsmx = (JSXT)dlsym(lj,"JSMX");
  EV provide = e->intern(e,"provide"); EV fset = e->intern(e,"fset");
  a[1] = e->make_function(e,0,0,jeini,"Create a J Engine",NULL);
  a[0] = e->intern(e,"j-engine"); e->funcall(e,fset,2,a);
  a[1] = e->make_function(e,2,2,jedo,"Execute a J Sentence",NULL);
  a[0] = e->intern(e,"j-do"); e->funcall(e,fset,2,a);
  a[1] = e->make_function(e,2,2,jesmx,"Set J i/o ",NULL);
  a[0] = e->intern(e,"j-smx"); e->funcall(e,fset,2,a);
  a[0] = e->intern(e, "jpl-module"); e->funcall(e,provide,1,a); R 0; }
