typedef int   C_INT;
typedef _Bool C_BOOL;
typedef float C_FLOAT;

struct base
{
   C_INT  i;
   C_BOOL b;
};

struct base2
{
   C_FLOAT r[3];
};

typedef struct base C_BASE;
typedef struct base2 C_BASE2;

C_INT getint (C_BASE *b)
{
   printf ("Structure b contains: %d, %d\n", (*b).i, b->b);
   return b->i;
}

C_FLOAT real (C_BASE2 b[3])
{
   printf("Here is b[0]: %f, %f, %f\n", b[0].r[0], b[0].r[1], b[0].r[2]);
   printf("Here is b[1]: %f, %f, %f\n", b[1].r[0], b[1].r[1], b[1].r[2]);
   printf("Here is b[2]: %f, %f, %f\n", b[2].r[0], b[2].r[1], b[2].r[2]);

   return b[0].r[0];
}
