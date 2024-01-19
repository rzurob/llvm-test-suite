! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/mProc/mProc/mProcDecRestrict1.f
! opt variations: -qck -qnok -qnol

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
!*  If a generic procedure is accessed from a module, the rules apply to all the
!*  specific versions even the rules apply to all the specific versions even
!*  if some of them are inaccessible by their specific names.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT(K1,N1)    ! (4,9)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    CHARACTER(N1) :: ID
  END TYPE

  INTERFACE Fun
    PROCEDURE ModFun
  END INTERFACE

  CONTAINS

  FUNCTION ModFun(arg)
  TYPE(DT(4,*)) :: Arg
  TYPE(DT(4,9))  :: ModFun
    ModFun%ID = "M-" // Arg%ID
  END FUNCTION

  END MODULE


  MODULE M1
  USE M, ONLY : DT

  TYPE, EXTENDS(DT) :: DT1(K2,N2)    ! (4,9,4,20)
      INTEGER, KIND :: K2
      INTEGER, LEN  :: N2
  END TYPE

  PROCEDURE(ModFun), POINTER :: ProcPtr

  INTERFACE Fun
    PROCEDURE ProcPtr
  END INTERFACE

  CONTAINS

  FUNCTION ModFun(arg)
  TYPE(DT1(4,*,4,*)) :: Arg
  TYPE(DT1(4,9,4,20))  :: ModFun
    ModFun%ID = "M1-" // Arg%ID
  END FUNCTION

  END MODULE

  MODULE M2
  USE M1

  CONTAINS

  SUBROUTINE ModSub()
    ProcPtr => ModFun
  END SUBROUTINE

  END MODULE

  PROGRAM  mProcDecRestrict1
  USE M, ONLY: Fun, DT
  USE M1, ONLY: Fun, DT1
  USE M2

  TYPE(DT(4,9))  :: T
  TYPE(DT1(4,9,4,20)) :: T1

  CALL ModSub

  T  = Fun(DT(4,9) ("0"))
  T1 = Fun(DT1(4,9,4,20)("1"))

  IF (TRIM(T%ID)   .NE. "M-0" )  ERROR STOP 11
  IF (TRIM(T1%ID)  .NE. "M1-1" ) ERROR STOP 12

  END

