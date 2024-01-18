!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : ofsk03f.f
!*
!*  PROGRAMMER                 : Jin Li
!*  DATE                       : 2010-09-30
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 Omit FUNCTION and SUBROUTINE Keywords
!*  REFERENCE                  : Feature Number 376084
!*  REQUIRED COMPILER OPTIONS  : noopt
!*
!*  DESCRIPTION
!*
!*  A simple program with a minimal module whose Function is terminated by "END"
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789
    MODULE test_mod
        CONTAINS
        FUNCTION sum(x, y)
        INTEGER x,y
        sum=x+y
        END

    END MODULE test_mod


    program test_a
    USE test_mod
    INTEGER :: A = 0
    A = sum(1, 2)
    print *, A

    END
