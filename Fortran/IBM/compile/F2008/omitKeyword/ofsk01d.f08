!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-09-30
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 Omit FUNCTION and SUBROUTINE Keywords
!*  REFERENCE                  : Feature Number 376084
!*  REQUIRED COMPILER OPTIONS  : noopt
!*
!*  DESCRIPTION
!*
!*  Error message expected when using "END blah" to terminate an internal
!*  function called "blah"
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789
    program test
    INTEGER :: A = 0
    A = blah(1, 2)
    print *, A

    CONTAINS
	FUNCTION blah(x, y)
	INTEGER x, y
	blah=x+y
	END blah

    END
