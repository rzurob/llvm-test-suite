! GB DTP extension using:
! ftcx_dtp -qck /tstdev/F2003/mProc/mProc/mProcDecRestrict.f
! opt variations: -qnock

!*********************************************************************
!*  ===================================================================
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
!*  If a generic procedure is accessed from a module, the rules apply to all the specific
!*  versions even if some of them are inaccessible by their specific names
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT(K1,N1)    ! (1,1)
    INTEGER, KIND             :: K1
    INTEGER, LEN              :: N1
    CHARACTER(kind=K1,len=N1) :: ID
  CONTAINS
    GENERIC    :: ASSIGNMENT(=) => ModSub
    PROCEDURE, PASS(Arg2)  :: ModSub
  END TYPE

  CONTAINS

  SUBROUTINE ModSub(Arg1, Arg2)
  TYPE(DT(1,*)),          INTENT(OUT)   :: Arg1
  CLASS(DT(1,*)),         INTENT(IN)    :: Arg2
  END SUBROUTINE

  SUBROUTINE ModSub1(Arg1, Arg2)
  TYPE(DT(1,*)),          INTENT(OUT)  :: Arg1
  CLASS(DT(1,*)),         INTENT(IN)   :: Arg2
  END SUBROUTINE

  END MODULE

  MODULE M1
  USE M, ONLY: DT, ModSub1

  INTERFACE ASSIGNMENT(=)
    PROCEDURE ModSub1
  END INTERFACE

  END MODULE

  MODULE M2
  USE M, ONLY : ModSub, ModSub1

  INTERFACE  ASSIGNMENT(=)
    PROCEDURE ModSub
    PROCEDURE ModSub1
  END INTERFACE

  END MODULE

  MODULE M3
  USE M

  PROCEDURE(ModSub), POINTER :: ProcPtr

  INTERFACE  ASSIGNMENT(=)
    PROCEDURE ProcPtr
  END INTERFACE

  END MODULE

  PROGRAM mProcDecRestrict

  END
