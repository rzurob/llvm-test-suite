typedef struct
{
   int i1;
   int i2;
}base;

void print1(base *t)
{
   printf ("Here is t->i1: %d\n", t->i1 );
}

void second(base *t)
{
   printf ("Here is t->i2: %d\n", t->i2 );
}

int firstget (base *t)
{
   return t->i1;
}

int secondget (base *t)
{
   return t->i2;
}

