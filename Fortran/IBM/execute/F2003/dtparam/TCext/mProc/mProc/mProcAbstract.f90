! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/mProc/mProc/mProcAbstract.f
! opt variations: -qnol

!*********************************************************************
!*  ===================================================================
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
!*  -- Proc defined with abstract interface
!*  ()
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

  INTERFACE Gname
    PROCEDURE ModFun
  END INTERFACE

  INTERFACE ABSTRACT
    FUNCTION F1(Arg1, Arg2)
      IMPORT
      TYPE(DT1(*,4)), INTENT(IN) :: Arg1
      TYPE(DT1(*,4)), INTENT(IN) :: Arg2
      TYPE(DT1(20,4))             :: F1
    END FUNCTION
  END INTERFACE

  INTERFACE ABSTRACT
    FUNCTION F2(Arg1, Arg2)
      IMPORT
      TYPE(DT2(*,4)), INTENT(IN) :: Arg1
      TYPE(DT2(*,4)), INTENT(IN) :: Arg2
      TYPE(DT2(20,4))             :: F2
    END FUNCTION
  END INTERFACE


  INTERFACE ABSTRACT
    FUNCTION F3(Arg1, Arg2)
      IMPORT
      TYPE(DT3(*,4)), INTENT(IN) :: Arg1
      TYPE(DT3(*,4)), INTENT(IN) :: Arg2
      TYPE(DT3(20,4))             :: F3
    END FUNCTION
  END INTERFACE


  CONTAINS

  FUNCTION ModFun(Arg1, Arg2)
  TYPE(DT(*,4)), INTENT(IN) :: Arg1
  TYPE(DT(*,4)), INTENT(IN) :: Arg2
  TYPE(DT(20,4))             :: ModFun
    ModFun%ID = Arg1%ID + Arg2%ID
  END FUNCTION

  FUNCTION ModFun1(Arg1, Arg2)
  TYPE(DT1(*,4)), INTENT(IN) :: Arg1
  TYPE(DT1(*,4)), INTENT(IN) :: Arg2
  TYPE(DT1(20,4))             :: ModFun1
    ModFun1%ID = Arg1%ID + Arg2%ID
  END FUNCTION

  FUNCTION ModFun2(Arg1, Arg2)
  TYPE(DT2(*,4)), INTENT(IN) :: Arg1
  TYPE(DT2(*,4)), INTENT(IN) :: Arg2
  TYPE(DT2(20,4))             :: ModFun2
    ModFun2%ID = Arg1%ID + Arg2%ID
  END FUNCTION

  END MODULE

  FUNCTION ExtFun(Arg1, Arg2)
  USE M
  TYPE(DT3(*,4)), INTENT(IN) :: Arg1
  TYPE(DT3(*,4)), INTENT(IN) :: Arg2
  TYPE(DT3(20,4))             :: ExtFun
    ExtFun%ID = Arg1%ID + Arg2%ID
  END FUNCTION


  PROGRAM mProcAbstract
  USE M
  USE M1

  PROCEDURE(F3) ExtFun


  INTERFACE  Gname
    PROCEDURE ExtFun
  END INTERFACE

  CALL IntSub(ModFun1)

  CONTAINS

  SUBROUTINE IntSub(Proc)
  PROCEDURE(F1)           :: Proc
  PROCEDURE(F2), POINTER  :: ProcPtr

  INTERFACE  Gname
    PROCEDURE Proc
  END INTERFACE

  INTERFACE Gname
    PROCEDURE ProcPtr
  END INTERFACE

  TYPE(DT(20,4))  :: T=DT(20,4)(-1)
  TYPE(DT1(20,4)) :: T1=DT1(20,4)(1)
  TYPE(DT2(20,4)) :: T2=DT2(20,4)(2)
  TYPE(DT3(20,4)) :: T3=DT3(20,4)(3)


  ProcPtr => ModFun2

  T  = Gname(DT(20,4)(-1), DT(20,4)(-1))
  T1 = Gname(DT1(20,4)(1), DT1(20,4)(1))
  T2 = Gname(DT2(20,4)(2), DT2(20,4)(2))
  T3 = Gname(DT3(20,4)(3), DT3(20,4)(3))

  IF (T%ID  .NE. -2 ) STOP 11
  IF (T1%ID  .NE. 2 ) STOP 12
  IF (T2%ID  .NE. 4 ) STOP 13
  IF (T3%ID  .NE. 6 ) STOP 14

  END  SUBROUTINE

  END

