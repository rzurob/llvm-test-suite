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
!*  A simple program with only one single internal procedure whose Function is
!*  terminated by "END"
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789
    program test
    INTEGER :: A = 0
    A = sum(1, 2)
    print *, A

    CONTAINS
	FUNCTION sum(x, y)
	INTEGER x, y
	sum=x+y
	END

    END
