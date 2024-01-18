! GB DTP extension using:
! ftcx_dtp -qck /tstdev/F2003/mProc/mProc/mProcDecRestrict7.f
! opt variations: -qnock

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcDecRestrict7.f
!*
!*  DATE                       : Mar 13, 2006
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
!*  One has a passed-object dummy arguments and the other does not have a passed obj
!*
!*  (317253)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(K1,N1)    ! (1,20)
    INTEGER, KIND             :: K1
    INTEGER, LEN              :: N1
    CHARACTER(kind=K1,len=N1) :: ID
    CONTAINS
    GENERIC ::  G => ModSub, ExtSub
    PROCEDURE, PASS(ARG2)  :: ModSub
    PROCEDURE, NOPASS      :: ExtSub
  END TYPE


  CONTAINS

  SUBROUTINE ModSub(Arg1, Arg2)
  TYPE(DT(1,*)), INTENT(INOUT) :: Arg1
  CLASS(DT(1,*)), INTENT(IN)    :: Arg2
    Arg1%ID = "ModSub-"//Arg2%ID
  END SUBROUTINE

  SUBROUTINE ExtSub(Arg1, Arg2)
  TYPE(DT(1,*)), INTENT(INOUT) :: Arg1
  TYPE(DT(1,*)), INTENT(IN)    :: Arg2
    Arg1%ID = "ExtSub-"//Arg2%ID
  END SUBROUTINE

  END MODULE

  PROGRAM mProcDecRestrict7
  USE M

  INTERFACE G
    PROCEDURE ModSub
  END INTERFACE


  TYPE(DT(1,20)) :: T, T1

  CALL G(T, DT(1,20)("0"))
  IF (TRIM(T%ID)    .NE. "ModSub-0"   ) STOP 11

   CALL T%G( T1 )
   IF (TRIM(T1%ID)    .NE. "ModSub-ModSub-0"   ) STOP 12

  CALL T%G(T1, DT(1,20)("1") )
  IF (TRIM(T1%ID)    .NE. "ExtSub-1"   ) STOP 13


  END



