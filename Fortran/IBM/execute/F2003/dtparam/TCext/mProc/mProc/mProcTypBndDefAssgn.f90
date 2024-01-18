! GB DTP extension using:
! ftcx_dtp -qck /tstdev/F2003/mProc/mProc/mProcTypBndDefAssgn.f
! opt variations: -qnock

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 01, 2006
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
!* Interaction with type bound generic
!*
!*  -- Defined assignment
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT(K1,N1)    ! (1,20)
    INTEGER, KIND             :: K1
    INTEGER, LEN              :: N1
    CHARACTER(kind=K1,len=N1) :: ID
    CONTAINS
    GENERIC    :: ASSIGNMENT(=) => ModSub
    PROCEDURE, PASS(ARG2)  :: ModSub
  END TYPE

  TYPE :: DT1(K2,N2)    ! (1,20)
   INTEGER, KIND             :: K2
   INTEGER, LEN              :: N2
   CHARACTER(kind=K2,len=N2) :: ID
  END TYPE

  TYPE :: DT2(K3,N3)    ! (1,20)
    INTEGER, KIND             :: K3
    INTEGER, LEN              :: N3
    CHARACTER(kind=K3,len=N3) :: ID
  END TYPE

  TYPE :: DT3(K4,N4)    ! (1,20)
    INTEGER, KIND             :: K4
    INTEGER, LEN              :: N4
    CHARACTER(kind=K4,len=N4) :: ID
  END TYPE

  CONTAINS

  SUBROUTINE ModSub(Arg1, Arg2)
  TYPE(DT(1,*)), INTENT(INOUT) :: Arg1
  CLASS(DT(1,*)), INTENT(IN)    :: Arg2
    Arg1%ID = "ModSub-"//Arg2%ID
  END SUBROUTINE

  SUBROUTINE ModSub1(Arg1, Arg2)
  TYPE(DT1(1,*)), INTENT(INOUT) :: Arg1
  CLASS(DT1(1,*)), INTENT(IN)    :: Arg2
    Arg1%ID =  "ModSub1-"//Arg2%ID
  END SUBROUTINE

  SUBROUTINE ModSub2(Arg1, Arg2)
  TYPE(DT2(1,*)), INTENT(INOUT) :: Arg1
  CLASS(DT2(1,*)), INTENT(IN)    :: Arg2
    Arg1%ID = "ModSub2-"//Arg2%ID
  END SUBROUTINE

  END MODULE

  SUBROUTINE ExtSub(Arg1, Arg2)
  USE M
  TYPE(DT3(1,*)), INTENT(INOUT) :: Arg1
  TYPE(DT3(1,*)), INTENT(IN)    :: Arg2
    Arg1%ID = "ExtSub-"//Arg2%ID
  END SUBROUTINE


  PROGRAM mProcTypBndDefAssgn
  USE M

  INTERFACE ASSIGNMENT(=)
    SUBROUTINE ExtSub(Arg1, Arg2)
      IMPORT
      TYPE(DT3(1,*)), INTENT(INOUT) :: Arg1
      TYPE(DT3(1,*)), INTENT(IN)    :: Arg2
    END SUBROUTINE
  END INTERFACE

  INTERFACE ASSIGNMENT(=)
    PROCEDURE ExtSub
!   PROCEDURE ModSub
  END INTERFACE

  CALL IntSub(ModSub1)

  CONTAINS

  SUBROUTINE IntSub(Proc)
  PROCEDURE(ModSub1)           :: Proc
  PROCEDURE(ModSub2), POINTER  :: ProcPtr

  INTERFACE ASSIGNMENT(=)
    PROCEDURE Proc
  END INTERFACE

  INTERFACE ASSIGNMENT(=)
    PROCEDURE ProcPtr
  END INTERFACE

  TYPE(DT(1,20))  :: T
  TYPE(DT1(1,20)) :: T1
  TYPE(DT2(1,20)) :: T2
  TYPE(DT3(1,20)) :: T3

  ProcPtr => ModSub2


  T  = DT(1,20)("0")
  T1 = DT1(1,20)("1")
  T2 = DT2(1,20)("2")
  T3 = DT3(1,20)("3")

  IF (TRIM(T%ID)    .NE. "ModSub-0"   ) STOP 11
  IF (TRIM(T1%ID)   .NE. "ModSub1-1"  ) STOP 12
  IF (TRIM(T2%ID)   .NE. "ModSub2-2"  ) STOP 13
  IF (TRIM(T3%ID)   .NE. "ExtSub-3"   ) STOP 14

  END  SUBROUTINE

  END

