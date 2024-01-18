! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/mProc/mProc/mProcDefOp.f
! opt variations: -qnok -qnol

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcDefOp.f
!*
!*  DATE                       : Mar 02, 2006
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
!*  -- Defined Operator
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: ID
  END TYPE

  TYPE, EXTENDS(DT) :: DT1(K2,N2)    ! (20,4,4,20)
      INTEGER, KIND :: K2
      INTEGER, LEN  :: N2
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K3,N3)    ! (20,4,4,20,4,20)
      INTEGER, KIND :: K3
      INTEGER, LEN  :: N3
  END TYPE

  TYPE, EXTENDS(DT2) :: DT3(K4,N4)    ! (20,4,4,20,4,20,4,20)
      INTEGER, KIND :: K4
      INTEGER, LEN  :: N4
  END TYPE

  END MODULE

  MODULE M1
  USE M

  INTERFACE OPERATOR( + )
    PROCEDURE ModFun
  END INTERFACE

  CONTAINS

  FUNCTION ModFun(Arg1, Arg2)
  TYPE(DT(*,4)), INTENT(IN) :: Arg1
  TYPE(DT(*,4)), INTENT(IN) :: Arg2
  TYPE(DT(20,4))             :: ModFun
    ModFun%ID = Arg1%ID + Arg2%ID
  END FUNCTION

  FUNCTION ModFun1(Arg1, Arg2)
  TYPE(DT1(*,4,4,*)), INTENT(IN) :: Arg1
  TYPE(DT1(*,4,4,*)), INTENT(IN) :: Arg2
  TYPE(DT1(20,4,4,20))             :: ModFun1
    ModFun1%ID = Arg1%ID + Arg2%ID
  END FUNCTION

  FUNCTION ModFun2(Arg1, Arg2)
  TYPE(DT2(*,4,4,*,4,*)), INTENT(IN) :: Arg1
  TYPE(DT2(*,4,4,*,4,*)), INTENT(IN) :: Arg2
  TYPE(DT2(20,4,4,20,4,20))             :: ModFun2
    ModFun2%ID = Arg1%ID + Arg2%ID
  END FUNCTION

  END MODULE

  FUNCTION ExtFun(Arg1, Arg2)
  USE M
  TYPE(DT3(*,4,4,*,4,*,4,*)), INTENT(IN) :: Arg1
  TYPE(DT3(*,4,4,*,4,*,4,*)), INTENT(IN) :: Arg2
  TYPE(DT3(20,4,4,20,4,20,4,20))             :: ExtFun
    ExtFun%ID = Arg1%ID + Arg2%ID
  END FUNCTION


  PROGRAM mProcDefOp
  USE M
  USE M1


  INTERFACE  OPERATOR( + )
    FUNCTION ExtFun(Arg1, Arg2)
      IMPORT
      TYPE(DT3(*,4,4,*,4,*,4,*)), INTENT(IN) :: Arg1
      TYPE(DT3(*,4,4,*,4,*,4,*)), INTENT(IN) :: Arg2
      TYPE(DT3(20,4,4,20,4,20,4,20))             :: ExtFun
    END FUNCTION
  END INTERFACE

  INTERFACE  OPERATOR( + )
    PROCEDURE ExtFun
  END INTERFACE

  CALL IntSub(ModFun1)

  CONTAINS

  SUBROUTINE IntSub(Proc)
  PROCEDURE(ModFun1)           :: Proc
  PROCEDURE(ModFun2), POINTER  :: ProcPtr

  INTERFACE  OPERATOR( + )
    PROCEDURE Proc
  END INTERFACE

  INTERFACE  OPERATOR( + )
    PROCEDURE ProcPtr
  END INTERFACE

  TYPE(DT(20,4)) :: T=DT(20,4)(-1)
  TYPE(DT1(20,4,4,20)) :: T1=DT1(20,4,4,20)(1)
  TYPE(DT2(20,4,4,20,4,20)) :: T2=DT2(20,4,4,20,4,20)(2)
  TYPE(DT3(20,4,4,20,4,20,4,20)) :: T3=DT3(20,4,4,20,4,20,4,20)(3)

  ProcPtr => ModFun2


  T  = DT(20,4)(-1) + DT(20,4)(-1)
  T1 = DT1(20,4,4,20)(1) + DT1(20,4,4,20)(1)
  T2 = DT2(20,4,4,20,4,20)(2) + DT2(20,4,4,20,4,20)(2)
  T3 = DT3(20,4,4,20,4,20,4,20)(3) + DT3(20,4,4,20,4,20,4,20)(3)

  IF (T%ID  .NE. -2 ) STOP 11
  IF (T1%ID  .NE. 2 ) STOP 12
  IF (T2%ID  .NE. 4 ) STOP 13
  IF (T3%ID  .NE. 6 ) STOP 14

  END  SUBROUTINE

  END

