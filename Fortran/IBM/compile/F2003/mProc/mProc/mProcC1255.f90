!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcC1255.f
!*
!*  DATE                       : Feb. 28, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Generalization of PROCEDURE statement
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
!*  C1255 (R1235) Within the subprogram containing the entry-stmt, the entry-name
!*  shall not appear as a dummy argument in the FUNCTION or SUBROUTINE statement
!*  or in another ENTRY statement nor shall it appear in an EXTERNAL, INTRINSIC,
!*  or PROCEDURE statement.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  CONTAINS

  RECURSIVE FUNCTION ModFun(Arg) RESULT(T)
  INTEGER :: Arg, T
  INTEGER(1) :: Arg1
  INTEGER(2) :: Arg2

  INTERFACE Fun
    PROCEDURE ModFun
    PROCEDURE ModFun1
    PROCEDURE ModFun2
  END INTERFACE

    T  = Arg
    RETURN
  ENTRY ModFun1(Arg1)
    T = Arg1 + 1_1
    RETURN
  ENTRY ModFun2(Arg2)
    T = Arg2 + 2_2
    RETURN
  END FUNCTION

  END MODULE


  RECURSIVE SUBROUTINE ExtSub()

  INTERFACE Fun
    PROCEDURE ExtSub
    PROCEDURE ExtSub1
  END INTERFACE

    RETURN
  ENTRY ExtSub1()
    RETURN
  END SUBROUTINE


  PROGRAM mProcC1255
  END


