// ****************************************************************************
//  ===========================================================================
//  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
//  ===========================================================================
//
//  TEST CASE NAME  : F2008/value/pure/unit_tests/func/purevalue11.c
//  TEST CASE TITLE : F2008: VALUE attr allowed for dummy args of PURE proc
//  PROGRAMMER      : Gaby Baghdadi
//  DATE            : 2010-12-01
//  ORIGIN          : XL Fortran Compiler Development, IBM Torolab
//  DRIVER STANZA   : xlc
//
//  DESCRIPTION
//  - used in conjunction with purevalue11.f
// ****************************************************************************

#include <stdio.h>
#include <stdlib.h>

extern int foo (int *);
extern void sub (int *);

int main()
{
    int oldv = 1;
    int res, saveold = oldv;

    printf("in C before foo, oldv=%d\n",oldv);
    res = foo(&oldv);
    printf("in C after foo, oldv=%d\n",oldv);
    if (oldv != saveold)
        exit(1);

    printf("in C before sub, oldv=%d\n",oldv);
    sub(&oldv);
    printf("in C after sub, oldv=%d\n",oldv);
    if (oldv != saveold)
        exit(2);

    exit(0);
}
