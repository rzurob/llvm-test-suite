void foo2(int *);
void foo5(int *);

void C_FOO1 (int *i)
{
   void (* funcptr1)(int *);

   printf(" inside foo1: %d\n", *i);

   funcptr1 = foo2;
   (*i)++;
   funcptr1(i);

}

void foo3 (int *i)
{
   void (* funcptr2)(int *);
   void foo4();

   printf(" inside foo3: %d\n", *i);

   funcptr2 = foo4;
   (*i)++;
   funcptr2(i);
}

void foo4 (int *i)
{
   void (* funcptr3)(int *);

   printf(" inside foo4: %d\n", *i);

   funcptr3 = foo5;
   (*i)++;
   funcptr3(i);
}
