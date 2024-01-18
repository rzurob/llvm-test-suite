// ****************************************************************************
//  ===========================================================================
//  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
//  ===========================================================================
//
//  TEST CASE NAME  : F2008/value/pure/unit_tests/func/purevalue10.c
//  TEST CASE TITLE : F2008: VALUE attr allowed for dummy args of PURE proc
//  PROGRAMMER      : Gaby Baghdadi
//  DATE            : 2010-12-01
//  ORIGIN          : XL Fortran Compiler Development, IBM Torolab
//  DRIVER STANZA   : xlc
//
//  DESCRIPTION
//  - used in conjunction with purevalue10.f
// ****************************************************************************

#include <stdio.h>
#include <stdlib.h>

struct dt0
{
    int a;
    short b;
};

struct dt1 
{
    int a;
    short b;
    struct dt0 d0;
};

struct dt2 
{
    int a;
    short b;
    struct dt1 d1;
};

extern int foo(struct dt2);

int main()
{
    struct dt0 dta;
    struct dt1 dtb;
    struct dt2 dtc;
    int ret;

    dtc.a = 1;
    dtc.b = 2;
    dtc.d1.a = 3;
    dtc.d1.b = 4;
    dtc.d1.d0.a = 5;
    dtc.d1.d0.b = 6;

    ret = foo(dtc);

    if (dtc.a != 1) exit(1);
    if (dtc.b != 2) exit(2);
    if (dtc.d1.a != 3) exit(3);
    if (dtc.d1.b != 4) exit(4);
    if (dtc.d1.d0.a != 5) exit(5);
    if (dtc.d1.d0.b != 6) exit(6);

    exit(ret);
}
