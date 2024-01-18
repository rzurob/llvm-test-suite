!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcSourceOfProc.f
!*
!*  DATE                       : Mar 07, 2006
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
!*  The source of procedures - generic bindings from parent type.
!*
!*  (317221)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT
    CHARACTER(20)  :: ID
  CONTAINS
    GENERIC    :: ASSIGNMENT(=) => ModSub
    PROCEDURE, PASS(Arg2)  :: ModSub
  END TYPE

  TYPE, EXTENDS(DT) :: DT1
  END TYPE

  TYPE(DT) T

  CONTAINS

  SUBROUTINE ModSub(Arg1, Arg2)
  TYPE(DT),          INTENT(OUT)   :: Arg1
  CLASS(DT),         INTENT(IN)    :: Arg2
    Arg1%ID = "M-" //Arg2%ID
  END SUBROUTINE

  END MODULE

  MODULE M1
  USE M, ONLY: DT

  PROCEDURE(ModSub), POINTER :: ProcPtr

  TYPE, EXTENDS(DT) :: DT1
  END TYPE

  INTERFACE ASSIGNMENT(=)
    PROCEDURE ProcPtr
  END INTERFACE

  TYPE(DT1) :: T1=DT1("0")
  TYPE(DT1) :: T2=DT1("2")

  CONTAINS

  SUBROUTINE ModSub(Arg1, Arg2)
  TYPE(DT1),          INTENT(OUT)   :: Arg1
  CLASS(DT1),         INTENT(IN)    :: Arg2
    Arg1%ID = "M1-" //Arg2%ID
  END SUBROUTINE

  SUBROUTINE IniTProcPtr()
    ProcPtr => ModSub
  END SUBROUTINE

  END MODULE

  PROGRAM  mProcDecRestrict1
  USE M, ONLY: T
  USE M1, ONLY: T1, T2, InitProcPtr, ASSIGNMENT(=)


  CALL InitProcPtr()

  T  = T1
  T1 = T2

  IF (TRIM(T%ID)   .NE. "M-0" )  STOP 11
  IF (TRIM(T1%ID)  .NE. "M1-2" ) STOP 12

  END

