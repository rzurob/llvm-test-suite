/*  Kobi Vinayagamoorthy
 This is modified code of precision_r*.f from /xlftest/src/zzrc/ to be able to compare real values in C
*/

/* --------------------------------------------------------------------
 float
-------------------------------------------------------------------- */


int precision_flt(float value,float exp)
{
    float range,high_a,low_a,temp;
    range = .00001f;

    temp = exp*range;
    high_a = temp + exp;
    low_a = exp - temp;

    if(exp < 0.0E+0f) 
    {
            if ( ((value >= high_a) && (value <= low_a)) == 1 )  return 1;
            else return 0;
    }
    else
    {
            if ( ((value <= high_a) && (value >= low_a)) == 1 )  return 1;
            else return 0;
    }

}

/* --------------------------------------------------------------------
// double
-------------------------------------------------------------------- */

int precision_dbl(double value,double exp)
{
    double range,high_a,low_a,temp;
    range = .000000000000010L;

    temp = exp*range;
    high_a = temp + exp;
    low_a = exp - temp;

    if(exp < 0.0E+0L)
    {
            if ( ((value >= high_a) && (value <= low_a)) == 1 )  return 1;
            else return 0;
    }
    else
    {
            if ( ((value <= high_a) && (value >= low_a)) == 1 )  return 1;
            else return 0;
    }

}


/* --------------------------------------------------------------------
// long double
-------------------------------------------------------------------- */

int precision_ldbl(long double value,long double exp)
{
    long double range,high_a,low_a,temp;
    range = .00000000000000000000000000000010L;

    temp = exp*range;
    high_a = temp + exp;
    low_a = exp - temp;

    if(exp < 0.0E+0L)
    {
            if ( ((value >= high_a) && (value <= low_a)) == 1 )  return 1;
            else return 0;
    }
    else
    {
            if ( ((value <= high_a) && (value >= low_a)) == 1 )  return 1;
            else return 0;
    }

}

