!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : ofsk02d.f
!*
!*  DATE                       : 2010-09-30
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 Omit FUNCTION and SUBROUTINE Keywords
!*  REFERENCE                  : Feature Number 376084
!*  REQUIRED COMPILER OPTIONS  : noopt
!*
!*  DESCRIPTION
!*
!*  Error message expected when using "END blah" to terminate an module
!*  subroutine called "sub"
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789
    MODULE test_mod
        CONTAINS
        SUBROUTINE sub
        INTEGER :: B = 1
        print *, B
        END blah

    END MODULE test_mod


    program test_a
    USE test_mod
    INTEGER :: A = 0

    print *, A
    CALL sub
    END
