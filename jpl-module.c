#define _GNU_SOURCE
#include <emacs-module.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <dlfcn.h>
#include <assert.h>
#include <signal.h>
#include <pthread.h>
typedef void V;typedef intmax_t I;typedef char C;typedef V* J;
typedef ptrdiff_t DP;typedef V* (*JIT)();typedef int (*JDT)(J,C*);
typedef C* (*JGT)(J);typedef V* (*JFT)(J);typedef V* (*JSXT) (J,V*,V*,V*,V*,I);
typedef emacs_value EV;typedef emacs_env EE;typedef struct emacs_runtime ERT;
#define R return
#define LIBJ "libj.so"
#define MTYOEXIT 5 // see jlib.h
#define EFUN(f) static EV f(EE*e,DP n,EV*a,V*d)
typedef struct {J j; C* speech; C*out;} JE;
int plugin_is_GPL_compatible;
static C **adadbreak;
static JDT jdo;static JFT jfree;static JIT jinit;static JSXT jsmx;static JGT jgetr;
static C* estring(EE* e, EV s)
{ DP sz=0;e->copy_string_contents(e,s,NULL,&sz);C *es=malloc(sz);
  e->copy_string_contents(e,s,es,&sz); R es; }
EFUN(jeinit)
{ J j = jinit();
  jsmx(j,NULL,NULL,NULL,NULL,2); // only 2 avoids J stack error
  R e->make_user_ptr(e,(V*)jfree,j); }
EFUN(jefree)
{ J j=e->get_user_ptr(e,a[0]);
  jfree(j);
  R e->make_integer(e,0); }
static V* jdoit (V*a) {
  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS,NULL);
  JE *je = (JE*)a;
  jdo((*je).j,(*je).speech);
  C*r = jgetr((*je).j);
  (*je).out = malloc(strlen(r));
  strcpy((*je).out,r);
  R NULL; }
EFUN(jeval)
{ JE je = {e->get_user_ptr(e,a[0]),estring(e,a[1]),NULL};
  pthread_t t;pthread_create (&t,NULL,jdoit,(V*)&je);
  struct timespec delay = {0,1000*1000};
  int flag=1;
  adadbreak = (C**) je.j;
  while(pthread_tryjoin_np(t,NULL)) {
    if(flag && e->should_quit(e)) {
      adadbreak += 1;
      flag--;
    }
    nanosleep(&delay,NULL);
  }
  EV r = e->make_string(e,je.out,strlen(je.out));
  free(je.speech);free(je.out);
  R r;
}
int emacs_module_init (ERT* rt)
{ EE* e = rt->get_environment(rt); EV a[2];
  EV provide = e->intern(e,"provide"); EV fset = e->intern(e,"fset");

  V* lj = dlopen(LIBJ,RTLD_LAZY);
  jinit = (JIT)dlsym(lj,"JInit"); jdo = (JDT)dlsym(lj,"JDo");
  jfree = (JFT)dlsym(lj,"JFree"); jsmx = (JSXT)dlsym(lj,"JSMX");
  jgetr = (JGT)dlsym(lj,"JGetR");

  a[1] = e->make_function(e,0,0,jeinit,"Create a J engine",NULL);
  a[0] = e->intern(e,"j-engine"); e->funcall(e,fset,2,a);

  a[1] = e->make_function(e,2,2,jeval,"Execute some J speech",NULL);
  a[0] = e->intern(e,"j-getr"); e->funcall(e,fset,2,a);

  a[1] = e->make_function(e,1,1,jefree,"Free a J engine ",NULL);
  a[0] = e->intern(e,"j-free"); e->funcall(e,fset,2,a);

  a[0] = e->intern(e, "jpl-module"); e->funcall(e,provide,1,a); R 0; }
