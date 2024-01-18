!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcDecRestrict2.f
!*
!*  DATE                       : Feb. 27, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Generaliztion of PROCEDURE statement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 296676
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Two dummy arguments are distinguishable if neither is a subroutine
!*  and neither is TKR compatible
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  PROCEDURE(S2), POINTER :: ProcPtr

  CONTAINS

  SUBROUTINE S1(Arg)
  END SUBROUTINE

  SUBROUTINE S2(Arg)
  PROCEDURE(INTEGER) :: Arg
  END SUBROUTINE

  END MODULE

  PROGRAM mProcDecRestrict2
  USE M

  CONTAINS

  SUBROUTINE IntSub(Proc)
  PROCEDURE(S1) :: Proc

  INTERFACE Amb1
    PROCEDURE ProcPtr, S2
  END INTERFACE

  INTERFACE Amb2
    PROCEDURE S1, Proc
  END INTERFACE

  INTERFACE NoAmb  ! this is fine
    PROCEDURE S1, S1, ProcPtr, ProcPtr
  END INTERFACE

  END SUBROUTINE

  END


