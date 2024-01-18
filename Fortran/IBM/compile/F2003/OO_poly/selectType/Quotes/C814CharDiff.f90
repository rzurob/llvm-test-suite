! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  redherring.f  
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: tcomp C814CharDiff.f 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : C814diff
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Dec. 2, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C814
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The selector is a char, and the type spec has the diff len param 
!*    (The error recovery does not care the line 58)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C814CharDiff
  IMPLICIT NONE

  CALL Sub("1234")

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*) :: Arg
 
  SELECT TYPE ( Arg )
    TYPE IS (CHARACTER(4))
      STOP 20
    TYPE IS (INTEGER(2))
      STOP 50
    TYPE IS (CHARACTER(*))
      STOP 50
    CLASS DEFAULT
      STOP 30
  END SELECT 
  STOP 40

  END SUBROUTINE

  END

