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
    TYPE :: DT
      INTEGER :: Id
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
  TYPE(DT) :: V

  TYPE(DT), POINTER :: U(:)
  TYPE(DT), ALLOCATABLE :: X(:)

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
  TYPE (DT), POINTER      :: V3(:)
  TYPE (DT), ALLOCATABLE  :: V4(:)
  CHARACTER(*), POINTER   :: V5

  IF (ASSOCIATED(V2))  STOP 13

  IF (ASSOCIATED(V3))  STOP 15

  IF (ALLOCATED(V4))   STOP 17

  IF (ASSOCIATED(V5))  STOP 18

  END SUBROUTINE

  END


