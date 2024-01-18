! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Implicit2.f 
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
!*  TEST CASE NAME             : Implicit 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 02, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Selector 
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
!*     
!* Implicit 
!* ()
!* 
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M
    TYPE  :: DT0
      CHARACTER(513) :: C0="0"
      CONTAINS
      PROCEDURE, PASS   :: SetObj
    END TYPE

    !TYPE, ABSTRACT, EXTENDS(DT0) :: DT1
    TYPE,  EXTENDS(DT0) :: DT1
      CHARACTER(1025) :: C1="1"
    END TYPE

    TYPE, EXTENDS(DT1) :: DT
      CHARACTER(2049) :: C2="2"
    END TYPE

    TYPE (DT), SAVE, TARGET :: V

    CONTAINS

    SUBROUTINE SetObj(Arg)
    CLASS(DT0) :: Arg
      Arg%C0 = "SetDT0"
    END SUBROUTINE 

  END MODULE

  PROGRAM Implicit2 
  USE M
  IMPLICIT CLASS(*)(U)

  CALL Sub(V)

  CONTAINS

  SUBROUTINE Sub(U)
 
  SELECT TYPE (U)
  CLASS IS (DT)
    IF (TRIM(V%C0) .NE. "0") STOP 20
    IF (TRIM(V%C1) .NE. "1") STOP 21
    IF (TRIM(V%C2) .NE. "2") STOP 22

    U%DT0%C0 ="?"
    U%DT1%C1 ="?"
    U%C2 ="?"
    
    IF (TRIM(V%C0) .NE. "?") STOP 30
    IF (TRIM(V%C1) .NE. "?") STOP 31
    IF (TRIM(V%C2) .NE. "?") STOP 32

  CLASS DEFAULT
    STOP 40
  END SELECT

  END SUBROUTINE

  END



