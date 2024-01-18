!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcC1207_4.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 28, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Generalization of PROCEDURE statement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 296676 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  
!*  C1207 (R1206) A procedure-name shall have an explicit interface and shall
!*  refer to an accessible procedure pointer, external procedure,
!*  dummy procedure, or module procedure.  
!*
!*  
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
  INTERFACE Fun 
    PROCEDURE ModFun, ModFun1, ModFun2 
  END INTERFACE

  CONTAINS

  FUNCTION ModFun(Arg) RESULT(T)
  INTEGER :: Arg, T 
  INTEGER(1) :: Arg1 
  INTEGER(2) :: Arg2
 
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


  FUNCTION ExtFun(Arg) 
  REAL       :: Arg, ExtFun 
  REAL(8)    :: Arg1, ExtFun1 
 
    ExtFun  = Arg
    RETURN
  ENTRY ExtFun1(Arg1)
    ExtFun1 = Arg1 + 1.0
    RETURN
  END FUNCTION


  PROGRAM mProcC1207_4
  USE M, ONLY: MFun => Fun

  INTERFACE Fun

    FUNCTION ExtFun(Arg)
      REAL       :: Arg, ExtFun
    END FUNCTION

    FUNCTION ExtFun1(Arg)
      REAL(8)   :: Arg, ExtFun1
    END FUNCTION

    PROCEDURE ExtFun, ExtFun1

  END INTERFACE

  IF (MFun(4)   .NE. 4 )   STOP 10
  IF (MFun(1_1) .NE. 2_1 ) STOP 11
  IF (MFun(2_2) .NE. 4_1 ) STOP 12

  IF (Fun(4.0)  .NE. 4.0 ) STOP 20
  IF (Fun(8._8) .NE. 9._8) STOP 21

  END


