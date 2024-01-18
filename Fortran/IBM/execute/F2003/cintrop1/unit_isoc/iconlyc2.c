_Bool foo(void (*(*arg)) (void))
{
  return (*arg == 0);
}
