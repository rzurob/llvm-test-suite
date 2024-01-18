_Bool foo(void ** arg)
{
  return (**((int**)arg) == 42);
}
