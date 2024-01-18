! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/mProc/mProc/mProcSpecificIntF.f
! opt variations: -ql

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcSpecificIntF.f
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
!*  Any procedure may be referenced via its specific interface if
!*  the specific interface is accessible.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    INTEGER(K1)   :: ID
  END TYPE

  TYPE, EXTENDS(DT) :: DT1    ! (4)
  END TYPE

  END MODULE

  MODULE M1
  USE M

  INTERFACE Fun
    PROCEDURE ModSub
  END INTERFACE

  CONTAINS

  SUBROUTINE ModSub(Arg1, Arg2)
  TYPE(DT(4)), INTENT(INOUT) :: Arg1
  TYPE(DT(4)), INTENT(IN)    :: Arg2
    Arg1 = Arg2
  END SUBROUTINE

  SUBROUTINE ModSub1(Arg1, Arg2)
  TYPE(DT1(4)), INTENT(INOUT) :: Arg1
  TYPE(DT1(4)), INTENT(IN)    :: Arg2
    Arg1 = Arg2
  END SUBROUTINE

  END MODULE


  PROGRAM mProcSpecificIntF
  USE M
  USE M1, ModSub11 => ModSub1, Fun1 => Fun, ModSub1=>ModSub1

  PROCEDURE(ModSub11), POINTER :: ProcPtr


  INTERFACE Fun1
    MODULE PROCEDURE ModSub1
    PROCEDURE        ModSub11
    PROCEDURE        ModSub1
  END INTERFACE

  INTERFACE Fun2
    PROCEDURE ModSub
    PROCEDURE ProcPtr
  END INTERFACE

  TYPE(DT(4))  :: T
  TYPE(DT1(4)) :: T1

  CALL Fun1(T, DT(4)(-1))
  CALL Fun1(T1, DT1(4)(1))

  IF (T%ID  .NE. -1 ) STOP 11
  IF (T1%ID  .NE. 1 ) STOP 12

  ProcPtr => ModSub1

  CALL Fun2(T, DT(4)(-2))
  CALL Fun2(T1, DT1(4)(2))

  IF (T%ID  .NE. -2 ) STOP 21
  IF (T1%ID  .NE. 2 ) STOP 22

  END

