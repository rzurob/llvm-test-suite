! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Misc10.f 
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc8 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 04, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : 
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
!*  Complain  :
!*  " The selector in the SELECT TYPE statement has a syntax error."
!*    (296497)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM  Misc10
  IMPLICIT NONE

  CLASS(*), POINTER :: Ptr

  ALLOCATE(Ptr, SOURCE=1_1 )

  SELECT TYPE ( As => (/Ptr/) )
    TYPE IS (INTEGER(1))
      IF ( ANY(AS .NE. 1_1 )) STOP 20
    CLASS DEFAULT
      STOP 30
  END SELECT

  END

