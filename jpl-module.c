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
typedef int (*JGMT)(J,C*,I*,I*,I*,I*);typedef int(*JSMT)(J,C*,I*,I*,I*,I*);
typedef emacs_value EV;typedef emacs_env EE;typedef struct emacs_runtime ERT;

#define EFUN(f) static EV f(EE*e,DP n,EV*a,V*d)
#define DO(n,x) {I i=0,_i=(n);for(;i<_i;++i){x;}}

static JDT jdo;static JFT jfree;static JIT jinit;static JSXT jsmx;
static JGT jgetr;static JGMT jgetm;static JSMT jsetm;

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
static EV jcopyb (EE*e,I r,I d)
{ EV v = blank_vector(e,r);
  DO(r,e->vec_set(e,v,i,e->make_integer(e,(int)((C*)d)[i]))); R v; }
static EV jcopyf (EE*e,I r,I d)
{ EV v = blank_vector(e,r);
  DO(r,e->vec_set(e,v,i,e->make_float(e,((D*)d)[i]))); R v; }
static EV jcopys (EE*e,I r,I d)
{ R e->make_string(e,(C*)d,r); }

EFUN(jeinit)
{ R e->make_user_ptr(e,(V*)jfree,jinit()); }
EFUN(jefree)
{ J j=e->get_user_ptr(e,a[0]); jfree(j); R e->make_integer(e,0); }
EFUN(jedo)
{ J j=e->get_user_ptr(e,a[0]);C*s=estring(e,a[1]);
  I r=jdo(j,s);free(s);R e->make_integer(e,r); }
EFUN(jegetr)
{ J j=e->get_user_ptr(e,a[0]);C*s=estring(e,a[1]);jdo(j,s);free(s);
  C*r=jgetr(j);R e->make_string(e,r,strlen(r)); }
EFUN(jesmx)
{ J j=e->get_user_ptr(e,a[0]);C*o=estring(e,a[1]);freopen(o,"a",stdout);free(o);
  jsmx(j,jputs,NULL,NULL,NULL,2); R e->make_integer(e,0); }
EFUN(jesetss)
{ J j=e->get_user_ptr(e,a[0]);C*var=estring(e,a[1]);C*val=estring(e,a[2]);
  I jt=2,jr=1,*jl=malloc(sizeof(I));*jl=strlen(val);
  I r = jsetm(j,var,&jt,&jr,(I*)&jl,(I*)&val);
  free(jl); R e->make_integer(e,r); }
EFUN(jesetiv) // for now, assumes rank 1 int vector
{ J j=e->get_user_ptr(e,a[0]);C*var=estring(e,a[1]);EV val=a[2];
  I jt=4,jr=1,*jl=malloc(sizeof(I));*jl=e->vec_size(e,val);
  I *jd = malloc(sizeof(I)*(*jl));
  DO(*jl,jd[i]=e->extract_integer(e,e->vec_get(e,val,i)));
  I r = jsetm(j,var,&jt,&jr,(I*)&jl,(I*)&jd);
  free(jl);free(jd); R e->make_integer(e,r); }
EFUN(jesetis) // int scalar
{ J j=e->get_user_ptr(e,a[0]);C*var=estring(e,a[1]);EV val=a[2];
  I jt=4,jr=0,*jl=NULL,*jd=malloc(sizeof(I));*jd=e->extract_integer(e,val);
  I r = jsetm(j,var,&jt,&jr,(I*)&jl,(I*)&jd);free(jd);R e->make_integer(e,r); }
EFUN(jesetfv) // for now, assumes rank 1 float vector
{ J j=e->get_user_ptr(e,a[0]);C*var=estring(e,a[1]);EV val=a[2];
  I jt=8,jr=1,*jl=malloc(sizeof(I));*jl=e->vec_size(e,val);
  D *jd = malloc(sizeof(D)*(*jl));
  DO(*jl,jd[i]=e->extract_float(e,e->vec_get(e,val,i)));
  I r = jsetm(j,var,&jt,&jr,(I*)&jl,(I*)&jd);
  free(jl);free(jd); R e->make_integer(e,r); }
EFUN(jesetfs) // float scalar
{ J j=e->get_user_ptr(e,a[0]);C*var=estring(e,a[1]);EV val=a[2];
  I jt=8,jr=0,*jl=NULL;D*jd=malloc(sizeof(D));*jd=e->extract_float(e,val);
  I r = jsetm(j,var,&jt,&jr,(I*)&jl,(I*)&jd);free(jd);R e->make_integer(e,r); }
EFUN(jegetm)
{ J j=e->get_user_ptr(e,a[0]);C*v=estring(e,a[1]);I jt,jr,js,jd;
  EV cons = e->intern(e,"cons");
  if (0!=jgetm(j,v,&jt,&jr,&js,&jd))
    { e->non_local_exit_signal(e,e->intern(e,"jget-error"),a[1]);
      R e->make_integer(e,1); }
  I c=cardinality(jr,js);
  if      (jt==1) a[1]=jcopyb(e,c,jd);
  else if (jt==2) a[1]=jcopys(e,c,jd);
  else if (jt==4) a[1]=jcopyi(e,c,jd);
  else if (jt==8) a[1]=jcopyf(e,c,jd);
  else
    { EV msg = e->make_string(e,"J type not implemented",22);
      e->non_local_exit_signal(e,e->intern(e,"jget-error"),msg);
      R e->make_integer(e,1); }
  a[0]=jcopyi(e,jr,js); R e->funcall(e,cons,2,a); }

int emacs_module_init (ERT* rt)
{ EE* e = rt->get_environment(rt); EV a[2]; V* lj = dlopen(LIBJ,RTLD_LAZY);
  jinit = (JIT)dlsym(lj,"JInit"); jdo = (JDT)dlsym(lj,"JDo");
  jfree = (JFT)dlsym(lj,"JFree"); jsmx = (JSXT)dlsym(lj,"JSMX");
  jgetr = (JGT)dlsym(lj,"JGetR"); jgetm = (JGMT)dlsym(lj,"JGetM");
  jsetm = (JSMT)dlsym(lj,"JSetM");
  EV provide = e->intern(e,"provide"); EV fset = e->intern(e,"fset");

  a[1] = e->make_function(e,0,0,jeinit,"Create a J engine",NULL);
  a[0] = e->intern(e,"j-engine"); e->funcall(e,fset,2,a);

  a[1] = e->make_function(e,2,2,jedo,"Execute a J sentence",NULL);
  a[0] = e->intern(e,"j-do"); e->funcall(e,fset,2,a);

  a[1] = e->make_function(e,2,2,jegetr,"Execute a J sentence & grab output",NULL);
  a[0] = e->intern(e,"j-getr"); e->funcall(e,fset,2,a);

  a[1] = e->make_function(e,2,2,jegetm,"J value -> emacs value",NULL);
  a[0] = e->intern(e,"j-getm"); e->funcall(e,fset,2,a);

  a[1] = e->make_function(e,3,3,jesetss,"emacs string -> J value",NULL);
  a[0] = e->intern(e,"j-setm-string"); e->funcall(e,fset,2,a);

  a[1] = e->make_function(e,3,3,jesetiv,"emacs int vector -> J value",NULL);
  a[0] = e->intern(e,"j-setm-int-vector"); e->funcall(e,fset,2,a);

  a[1] = e->make_function(e,3,3,jesetis,"emacs int scalar -> J value",NULL);
  a[0] = e->intern(e,"j-setm-int"); e->funcall(e,fset,2,a);

  a[1] = e->make_function(e,3,3,jesetfv,"emacs float vector -> J value",NULL);
  a[0] = e->intern(e,"j-setm-float-vector"); e->funcall(e,fset,2,a);

  a[1] = e->make_function(e,3,3,jesetfs,"emacs float scalar -> J value",NULL);
  a[0] = e->intern(e,"j-setm-float"); e->funcall(e,fset,2,a);

  a[1] = e->make_function(e,2,2,jesmx,"Set J i/o ",NULL);
  a[0] = e->intern(e,"j-smx"); e->funcall(e,fset,2,a);

  a[1] = e->make_function(e,1,1,jefree,"Free J engine ",NULL);
  a[0] = e->intern(e,"j-free"); e->funcall(e,fset,2,a);

  a[0] = e->intern(e, "jpl-module"); e->funcall(e,provide,1,a); R 0; }
