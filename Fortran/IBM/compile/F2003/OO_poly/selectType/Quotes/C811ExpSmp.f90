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
! %POSTCMD: tcomp C811ExpSmp.f 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : C811ExpSmp
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Dec. 2, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C811 
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
!*    The selector is a simple integer expression without associate name. 
!*    (The err msg:  
!*    line 48.18: 1511-103 (S) The selector in the SELECT TYPE statement has a syntax error
!*    seems redundant)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C811ExpSmp
  IMPLICIT NONE
  
  SELECT TYPE (  1_1 - 2_2 )
    TYPE IS (CHARACTER(*))
      STOP 20
    TYPE IS (INTEGER(2))
      STOP 50
    CLASS DEFAULT
      STOP 30
  END SELECT 
  STOP 40

  END

