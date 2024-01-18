!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : submodule13d
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bernard Kan
!*  DATE                       : 6 December, 2012
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : submodule
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2008
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*    The name of the submodule in the end statement can be optionally
!*  specified.
!*  Positive diagnostic: no error if unspecified
!*                       no error if correctly specified
!*  Negative diagnostic: severe error if incorrectly specified
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!234567891323456789132345678913234567891323456789132345678913234567890

MODULE m
END MODULE m

! no error
SUBMODULE (m) b
END SUBMODULE b

! no error
SUBMODULE (m) c
END SUBMODULE

! error
SUBMODULE (m) c
END SUBMODULE d

! error
SUBMODULE (m) e
END SUBMODULE f

PROGRAM submodule13d
USE m
END PROGRAM submodule13d
