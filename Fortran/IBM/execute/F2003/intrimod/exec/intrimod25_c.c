int ieee_arithmetic[5];
float iso_c_binding;

int c_fun1()
{
   int i;
   
   for (i=0; i<5; i++)
      if (ieee_arithmetic[i] != 0) return 1;
   
   return 0;
}


int c_fun2()
{
    if ( iso_c_binding != 1.0 ) return 1;

    return 0;
}
