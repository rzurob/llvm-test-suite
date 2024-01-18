! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f 
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: tcomp C815TypeIsBindC.f 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : C815TypeIsBindC
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Dec. 2, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C815
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
!*   Use type with bind(c)  as type spec  
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C815TypeIsBindC
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

  TYPE, BIND(C) :: Bindc
    INTEGER(C_INT) :: i=1
    REAL(C_FLOAT)  :: r=1.0
  END TYPE

  TYPE(Bindc) :: Arg

  CALL Sub(Arg)

  CONTAINS

  SUBROUTINE Sub(Arg)

  CLASS(*) :: Arg
 
  SELECT TYPE ( Arg )
    TYPE IS (Bindc)
    CLASS IS (Bindc)
    CLASS DEFAULT
      STOP 30
  END SELECT 

  END SUBROUTINE
  END

