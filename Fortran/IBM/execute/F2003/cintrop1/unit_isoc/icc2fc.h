void * malloc(unsigned long);
void free(void *);
int main()
{
  TDEF_STMT;
  void foo(TDEF_NAME);
  TDEF_NAME arg;
  TARGET_DECL;
  MALLOC_CALL;
  DEREF arg = VALUE;
  foo(arg);
  FREE_CALL;
  return 0;
}
