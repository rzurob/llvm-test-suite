! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/mProc/mProc/mProcDefOp1.f
! opt variations: -qnol

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 09, 2006
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
!*  A generic interface block specifies a generic interface for each of the
!*  procedures in the interface block. The PROCEDURE statement lists procedure
!*  pointers, external procedures, du mmy procedures, or module procedures
!*  that have this generic interface. A generic interface is always explicit.
!*  -- Defined Unary Operator
!*  (323965)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: ID
  END TYPE

  TYPE, EXTENDS(DT) :: DT1    ! (20,4)
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2    ! (20,4)
  END TYPE

  TYPE, EXTENDS(DT2) :: DT3    ! (20,4)
  END TYPE

  END MODULE

  MODULE M1
  USE M

  INTERFACE OPERATOR( .OP. )
    PROCEDURE ModFun
  END INTERFACE

  CONTAINS

  FUNCTION ModFun(Arg)
  TYPE(DT(*,4)), INTENT(IN) :: Arg
  TYPE(DT(20,4))             :: ModFun
    ModFun%ID = -1 * Arg%ID
  END FUNCTION

  FUNCTION ModFun1(Arg)
  TYPE(DT1(*,4)), INTENT(IN) :: Arg
  TYPE(DT1(20,4))             :: ModFun1
    ModFun1%ID = 2 * Arg%ID
  END FUNCTION

  FUNCTION ModFun2(Arg)
  TYPE(DT2(*,4)), INTENT(IN) :: Arg
  TYPE(DT2(20,4))             :: ModFun2
    ModFun2%ID = 3 * Arg%ID
  END FUNCTION

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(DT3(*,4)), INTENT(IN) :: Arg
  TYPE(DT3(20,4))             :: ExtFun
    ExtFun%ID = 4 * Arg%ID
  END FUNCTION


  PROGRAM mProcDefOp1
  USE M
  USE M1


  INTERFACE  OPERATOR( .OP. )
    FUNCTION ExtFun(Arg)
      IMPORT
      TYPE(DT3(*,4)), INTENT(IN) :: Arg
      TYPE(DT3(20,4))             :: ExtFun
    END FUNCTION
  END INTERFACE

  INTERFACE  OPERATOR( .OP. )
    PROCEDURE ExtFun
  END INTERFACE

  CALL IntSub(ModFun1)

  CONTAINS

  SUBROUTINE IntSub(Proc)
  PROCEDURE(ModFun1)           :: Proc
  PROCEDURE(ModFun2), POINTER  :: ProcPtr

  INTERFACE  OPERATOR( .OP. )
    PROCEDURE Proc
  END INTERFACE

  INTERFACE  OPERATOR( .OP. )
    PROCEDURE ProcPtr
  END INTERFACE

  TYPE(DT(20,4))  :: T=DT(20,4)(-1)
  TYPE(DT1(20,4)) :: T1=DT1(20,4)(1)
  TYPE(DT2(20,4)) :: T2=DT2(20,4)(2)
  TYPE(DT3(20,4)) :: T3=DT3(20,4)(3)

  ProcPtr => ModFun2


  T  = .OP. DT(20,4)(-1)
  T1 = .OP. DT1(20,4)(1)
  T2 = .OP. DT2(20,4)(2)
  T3 = .OP. DT3(20,4)(3)

  IF (T%ID   .NE. 1 ) STOP 11
  IF (T1%ID  .NE. 2 ) STOP 12
  IF (T2%ID  .NE. 6 ) STOP 13
  IF (T3%ID  .NE. 12) STOP 14

  END  SUBROUTINE

  END

