! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_procptr/CrossFeatures2/Null6.f
! opt variations: -qnok -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 11, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Null
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   null()
!*   If any type parameters of the contextual entity are assumed,
!*   MOLD shall be present- procptr is not a data entity.
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
  END TYPE

  TYPE, EXTENDS(DT) :: DT1    ! (4,20)
    PROCEDURE(), POINTER, NOPASS :: ProcPtr
  END TYPE

  END MODULE

  PROGRAM Null6
  USE M
  IMPLICIT NONE

  INTERFACE
    FUNCTION IFun1(Arg)
      IMPORT DT
      CLASS(DT(4,*)) :: Arg
      CLASS(*), ALLOCATABLE :: IFun1(:)
    END FUNCTION
    FUNCTION IFun2(Arg)
      IMPORT DT
      CLASS(DT(4,*)) :: Arg
      CLASS(*), POINTER :: IFun2(:)
    END FUNCTION
  END INTERFACE

  PROCEDURE(IFun1), POINTER :: ProcPtr1
  PROCEDURE(IFun2), POINTER :: ProcPtr2
  PROCEDURE(     ), POINTER :: ProcPtr3

  TYPE(DT1(4,20)) :: V

  V = DT1(4,20)(NULL())
  IF (ASSOCIATED(V%ProcPtr))  ERROR STOP 13

  V = DT1(4,20)(NULL(ProcPtr3))
  IF (ASSOCIATED(V%ProcPtr))  ERROR STOP 14

  CALL IntSub(NULL(), NULL())
  CALL IntSub(NULL(ProcPtr1), NULL(ProcPtr2))

  CONTAINS

  SUBROUTINE IntSub(Arg1, Arg2)
  PROCEDURE(IFun1), POINTER :: Arg1
  PROCEDURE(IFun2), POINTER :: Arg2

  IF (ASSOCIATED(Arg1))  ERROR STOP 11
  IF (ASSOCIATED(Arg2))  ERROR STOP 12

  END SUBROUTINE

  END


