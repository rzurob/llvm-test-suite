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
!*   MOLD shall also be present if the reference appears as an actual argument
!*   corresponding to a  dummy argument with assumed character length.
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    INTERFACE
      SUBROUTINE fSub(Arg)
      CHARACTER(*), POINTER :: Arg
      END SUBROUTINE
    END INTERFACE

    TYPE :: DT
      INTEGER :: Id
      PROCEDURE(FSub), POINTER, NOPASS :: ProcPtr=>NULL()
    END TYPE

  CONTAINS

    SUBROUTINE IntSub(Arg)
    CHARACTER(*), POINTER :: Arg

    IF ( ASSOCIATED(Arg)) ERROR STOP 11
    IF (LEN(Arg) .NE. 3 ) ERROR STOP 12

    END SUBROUTINE

  END MODULE

  PROGRAM Null8
  USE M
  IMPLICIT NONE
  TYPE(DT) :: V
  CHARACTER(3), POINTER      :: CPtr=>NULL()
  PROCEDURE(IntSub), POINTER :: ProcPtr
  CHARACTER(3), TARGET       :: CTar="123"

  CALL IntSub(NULL(CPtr))

  ProcPtr => IntSub
  CALL ProcPtr(NULL(CPtr))

  V%ProcPtr => IntSub
  CALL V%ProcPtr(NULL(CPtr))

  ProcPtr => IntSub
  CPtr => CTar
  CALL ProcPtr(NULL(CPtr))

  END


