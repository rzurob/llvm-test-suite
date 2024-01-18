!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : ofsk02f.f
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
!*  A simple program with only one single internal procedure whose Subroutine is
!*  terminated by "END"
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789
    program test

    INTEGER :: A = 0
    CALL SUB
    print *, A
    print *, B

    CONTAINS
    SUBROUTINE SUB
        INTEGER :: B = 1
    END

    END
