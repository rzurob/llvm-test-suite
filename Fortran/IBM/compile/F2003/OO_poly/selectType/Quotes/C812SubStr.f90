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
! %POSTCMD: tcomp C812SubStr.f 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : C812SubStr
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Dec. 3, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C812 
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
!*    The selector is a substring appearing in variable definition context.
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C812SubStr
  IMPLICIT NONE

  CALL Sub( "0123456789" )

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*), INTENT(IN) :: Arg
  
  SELECT TYPE ( As => Arg )
    CLASS DEFAULT
      SELECT TYPE ( As => As )
        CLASS DEFAULT
        As = "9876543210" 
      END SELECT 
    TYPE IS (REAL(8))
      STOP 20
    TYPE IS (CHARACTER(*))
      STOP 30
  END SELECT 
  STOP 40

  END SUBROUTINE

  END

