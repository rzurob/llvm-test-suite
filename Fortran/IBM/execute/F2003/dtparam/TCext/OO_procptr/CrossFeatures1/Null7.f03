! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp /tstdev/OO_procptr/CrossFeatures1/Null7.f
! opt variations: -qnol -qdeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 11, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
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
!*   MOLD shall be present
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id=-1
      PROCEDURE(INTEGER), POINTER, NOPASS :: ProcPtr=>NULL()
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    INTEGER :: Fun
    INTEGER :: Arg
      Fun = Arg
    END FUNCTION

  END MODULE

  PROGRAM Null7
  USE M
  IMPLICIT NONE
  TYPE(DT(20,4)), POINTER :: V,W(:), U(:), X(:)

  INTERFACE ExtSub
    SUBROUTINE ExtSub(V1, V2, V3, V4)
      IMPORt
      TYPE (DT(*,4)), POINTER :: V1(:)
      TYPE (DT(*,4)), POINTER :: V2(:)
      TYPE (DT(*,4)), POINTER :: V3(:)
      TYPE (DT(*,4)), POINTER :: V4(:)
    END SUBROUTINE
  END INTERFACE

  ALLOCATE(W(3))
  ALLOCATE(U(1:0))
  ALLOCATE(X(-1:0))

  CALL ExtSub( NULL(U),   &
             & NULL(W), &
             & NULL(X), &
             & X  )


  END


  SUBROUTINE ExtSub(V1, V2, V3, V4)
  USE M
  TYPE (DT(*,4)), POINTER :: V1(:)
  TYPE (DT(*,4)), POINTER :: V2(:)
  TYPE (DT(*,4)), POINTER :: V3(:)
  TYPE (DT(*,4)), POINTER :: V4(:)


  IF (ASSOCIATED(V1))  ERROR STOP 11
! IF (SIZE(V1) .NE. 0) ERROR STOP 12

  IF (ASSOCIATED(V2))  ERROR STOP 13
! IF (SIZE(V2) .NE. 0) ERROR STOP 14

  IF (ASSOCIATED(V3))  ERROR STOP 15
! IF (SIZE(V3) .NE. 0) ERROR STOP 16

  IF ( .NOT. ASSOCIATED(V4))         ERROR STOP 17
  IF (SIZE(V4)       .NE. 2)         ERROR STOP 18
  IF (ANY(LBOUND(V4) .NE. (/-1/)) )  error sTOP 19
  END SUBROUTINE



