#ifdef FUNCPTR
#define ARGDCL FUNCPTR
#else
#define ARGDCL ARGTYPE ARGNAME
#endif
_Bool foo(ARGDCL)
{
  return (COMPARISON);
}
