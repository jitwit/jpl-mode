int plugin_is_GPL_compatible;
#include <emacs-module.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <dlfcn.h>
#define R return
#define LIBJ "libj.so"
#define MTYOEXIT 5 // see jlib.h

typedef void V;typedef intmax_t I;typedef char C;typedef double D;typedef V* J;
typedef ptrdiff_t DP;typedef V* (*JIT)();typedef int (*JDT)(J,C*);
typedef C* (*JGT)(J);typedef V* (*JFT)(J);typedef V* (*JSXT) (J,V*,V*,V*,V*,I);
typedef int (*JGMT)(J,C*,I*,I*,I*,I*);
typedef emacs_value EV;typedef emacs_env EE;typedef struct emacs_runtime ERT;

#define EFUN(f) static EV f(EE*e,DP n,EV*a,V*d)
#define TI(v) I*v=malloc(sizeof(I)*1)
#define DO(n,x) {I i=0,_i=(n);for(;i<_i;++i){x;}}

static JDT jdo;static JFT jfree;static JIT jinit;static JSXT jsmx;
static JGT jgetr;static JGMT jgetm;

static C*estring(EE* e, EV s)
{ DP sz=0; e->copy_string_contents(e,s,NULL,&sz);
  C *es=malloc(sz); e->copy_string_contents(e,s,es,&sz); R es; }
static V jputs (J j,int t,C*s)
{ if(MTYOEXIT==t) exit((int)(I)s); fputs(s,stdout); fflush(stdout); }
static I cardinality (I r,I s) { I c=1;DO(r,c*=((I*)s)[i]);R c; }
static EV blank_vector(EE*e,I c)
{ EV a[2],vec=e->intern(e,"make-vector");
  a[0]=e->make_integer(e,c);a[1]=e->make_integer(e,0);
  R e->funcall(e,vec,2,a); }
static EV jcopyi (EE*e,I r,I d)
{ EV v = blank_vector(e,r);
  DO(r,e->vec_set(e,v,i,e->make_integer(e,((I*)d)[i]))); R v; }
static EV jcopyf (EE*e,I r,I d)
{ EV v = blank_vector(e,r);
  DO(r,e->vec_set(e,v,i,e->make_float(e,((D*)d)[i]))); R v; }
static EV jcopys (EE*e,I r,I d)
{ R e->make_string(e,(C*)d,r); }

EFUN(jeinit)
{ R e->make_user_ptr(e,(V*)jfree,jinit()); }
EFUN(jefree)
{ J j=e->get_user_ptr(e,a[0]); jfree(j); R e->make_integer(e,0); }
EFUN(jegetr)
{ J j=e->get_user_ptr(e,a[0]);C*s=estring(e,a[1]);jdo(j,s);free(s);
  C*r=jgetr(j);R e->make_string(e,r,strlen(r)); }
EFUN(jesmx)
{ J j=e->get_user_ptr(e,a[0]);C*o=estring(e,a[1]);freopen(o,"a",stdout);free(o);
  jsmx(j,jputs,NULL,NULL,NULL,2); R e->make_integer(e,0); }
EFUN(jegetm) // nasty mess for now
{ J j=e->get_user_ptr(e,a[0]);C*v=estring(e,a[1]);I jt,jr,js,jd;
  EV cons = e->intern(e,"cons"); EV ed = e->intern(e,"nil"); EV ea[2];
  jgetm(j,v,&jt,&jr,&js,&jd); I c=cardinality(jr,js);
  if (jt==2) { // jlit
    ea[0]=jcopys(e,c,jd);ea[1]=ed;ed=e->funcall(e,cons,2,ea);
  } else if (jt==4) { // jint
    ea[0]=jcopyi(e,c,jd);ea[1]=ed;ed=e->funcall(e,cons,2,ea);
  } else if (jt==8) { // jfloat
    ea[0]=jcopyf(e,c,jd);ea[1]=ed;ed=e->funcall(e,cons,2,ea);
  } else {
    ea[0]=e->make_string(e,"todo",4);ea[1]=ed;ed=e->funcall(e,cons,2,ea);
  }
  ea[0]=jcopyi(e,jr,js);ea[1]=ed;ed=e->funcall(e,cons,2,ea);R ed; }

int emacs_module_init (ERT* rt)
{ EE* e = rt->get_environment(rt); EV a[2]; V* lj = dlopen(LIBJ,RTLD_LAZY);
  jinit = (JIT)dlsym(lj,"JInit"); jdo = (JDT)dlsym(lj,"JDo");
  jfree = (JFT)dlsym(lj,"JFree"); jsmx = (JSXT)dlsym(lj,"JSMX");
  jgetr = (JGT)dlsym(lj,"JGetR"); jgetm = (JGMT)dlsym(lj,"JGetM");
  EV provide = e->intern(e,"provide"); EV fset = e->intern(e,"fset");

  a[1] = e->make_function(e,0,0,jeinit,"Create a J engine",NULL);
  a[0] = e->intern(e,"j-engine"); e->funcall(e,fset,2,a);

  a[1] = e->make_function(e,2,2,jegetr,"Execute a J sentence",NULL);
  a[0] = e->intern(e,"j-getr"); e->funcall(e,fset,2,a);

  a[1] = e->make_function(e,2,2,jegetm,"J value -> emacs value",NULL);
  a[0] = e->intern(e,"j-getm"); e->funcall(e,fset,2,a);

  a[1] = e->make_function(e,2,2,jesmx,"Set J i/o ",NULL);
  a[0] = e->intern(e,"j-smx"); e->funcall(e,fset,2,a);

  a[1] = e->make_function(e,1,1,jefree,"Free J engine ",NULL);
  a[0] = e->intern(e,"j-free"); e->funcall(e,fset,2,a);

  a[0] = e->intern(e, "jpl-module"); e->funcall(e,provide,1,a); R 0; }
