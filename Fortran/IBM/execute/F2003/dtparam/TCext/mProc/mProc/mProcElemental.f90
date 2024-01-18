! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/mProc/mProc/mProcElemental.f
! opt variations: -qnol

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcElemental.f
!*
!*  DATE                       : Mar 03, 2006
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
!*  Elemental
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: ID
  END TYPE

  INTERFACE Fun
    PROCEDURE ModFun
  END INTERFACE

  PRIVATE ModFun

  CONTAINS

  ELEMENTAL FUNCTION ModFun(Arg)
  CLASS(DT(*,4)), INTENT(IN)   :: Arg
  TYPE(DT(20,4))  :: ModFun
    ModFun =  Arg
  END FUNCTION

  ELEMENTAL FUNCTION ModFun1(Arg, Arg1)
  CLASS(DT(*,4)), INTENT(IN) :: Arg, Arg1
  TYPE(DT(20,4))  :: ModFun1
    ModFun1 = Arg
  END FUNCTION

  END MODULE

  ELEMENTAL FUNCTION ExtFun(Arg, Arg1)
  USE M, ONLY : DT
  CLASS(DT(*,4)), INTENT(IN) :: Arg, Arg1
  TYPE(DT(20,4))  :: ExtFun
    ExtFun = Arg
  END FUNCTION


  PROGRAM mProcElemental
  USE M

  PROCEDURE(ModFun1) :: ExtFun

  INTERFACE Fun
    PROCEDURE ExtFun
  END INTERFACE

  TYPE(DT(20,4)) :: T(1000), T1(1000)
  INTEGER  :: I

  T  = Fun((/(DT(20,4)(I), I=1, 1000)/))
  T1 = Fun((/(DT(20,4)(I), I=11, 1010)/), T)

  IF ( ANY(T%ID  .NE. (/(I, I=1, 1000)/)) )  STOP 11
  IF ( ANY(T1%ID  .NE. (/(I, I=11, 1010)/)) )  STOP 12


  END

