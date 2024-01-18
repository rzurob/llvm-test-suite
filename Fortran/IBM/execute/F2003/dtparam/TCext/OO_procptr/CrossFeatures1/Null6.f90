! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp /tstdev/OO_procptr/CrossFeatures1/Null6.f
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
      INTEGER(K1)   :: Id
      PROCEDURE(Fun), NoPass, POINTER :: ProcPtr=>NULL()
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    INTEGER :: Fun
    INTEGER :: Arg
      Fun = Arg
    END FUNCTION

  END MODULE

  PROGRAM Null6
  USE M
  IMPLICIT NONE
  TYPE(DT(20,4)) :: V

  TYPE(DT(20,4)), POINTER :: U(:)
  TYPE(DT(20,4)), ALLOCATABLE :: X(:)

  CHARACTER(3), POINTER :: CPtr
  CHARACTER(3), TARGET  :: CTar="IBM"

  CPtr => CTar
  ALLOCATE(U(3))

  CALL IntSub(                  &
             & NULL(V%ProcPtr), &
             & NULL(U),         &
             & NULL(X),         &
             & NULL(CPtr)       )


  CONTAINS

  SUBROUTINE IntSub(V2, V3, V4, V5)
  PROCEDURE(Fun), POINTER :: V2
  TYPE (DT(*,4)), POINTER      :: V3(:)
  TYPE (DT(*,4)), ALLOCATABLE  :: V4(:)
  CHARACTER(*), POINTER   :: V5

  IF (ASSOCIATED(V2))  STOP 13

  IF (ASSOCIATED(V3))  STOP 15

  IF (ALLOCATED(V4))   STOP 17

  IF (ASSOCIATED(V5))  STOP 18

  END SUBROUTINE

  END


