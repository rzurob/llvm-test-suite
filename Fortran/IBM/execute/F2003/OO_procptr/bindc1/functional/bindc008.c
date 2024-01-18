
typedef struct
{
      int i;
      float d;
      char c[4];
} cbc;

void c_PrInT ( cbc *dtv )
{
   printf ("Here is the content: %d, %8.3f, %c%c%c%c\n", dtv->i, dtv->d, dtv->c[0], dtv->c[1], dtv->c[2], dtv->c[3]);
}

cbc CCombine ( cbc *dtv1, cbc *dtv2 )
{
   cbc tmp;
   tmp.i  = ( dtv1->i + dtv2->i );
   tmp.d  = ( dtv1->d + dtv2->d );
   tmp.c[0]  = dtv1->c[0]; 
   tmp.c[1]  = dtv1->c[1]; 
   tmp.c[2]  = dtv2->c[2]; 
   tmp.c[3]  = dtv2->c[3]; 
   return tmp;
}
