// ****************************************************************************
//  ===========================================================================
//  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
//  ===========================================================================
//
//  TEST CASE NAME  : F2008/value/pure/unit_tests/func/purevalue12.c
//  TEST CASE TITLE : F2008: VALUE attr allowed for dummy args of PURE proc
//  PROGRAMMER      : Gaby Baghdadi
//  DATE            : 2010-12-01
//  ORIGIN          : XL Fortran Compiler Development, IBM Torolab
//  DRIVER STANZA   : xlc
//
//  DESCRIPTION
//  - used in conjunction with purevalue12.f
// ****************************************************************************

#include <stdio.h>

int cfunc ( int arg )
{
    printf("in cfunc; arg=%d\n",arg);
    return arg;
}

int main()
{
    extern int foo (int(**)(int));
    extern void sub (int(**)(int));
    int (*pcfunc)(int) = cfunc;
    long oldv = (long)pcfunc;
    (*pcfunc)(123);

    printf("in C before foo, oldv=%d\n",(long)pcfunc);
    int res = foo(&pcfunc);
    printf("in C after foo, oldv=%d\n",(long)pcfunc);
    if (oldv != (long)pcfunc)
        exit(1);

    printf("in C before sub, oldv=%d\n",(long)pcfunc);
    sub(&pcfunc);
    printf("in C after sub, oldv=%d\n",(long)pcfunc);
    if (oldv != (long)pcfunc)
        exit(2);

    exit(0);
}
