int f1_cB (int j)
{
  return 4;
}

extern void f1_fB (float);
void f1f_via_c ()
{
   f1_fB (3.0);
}


main ()
{
    f1f_via_c ();
    f1c_via_f ();
    f2f_via_f ();
}
