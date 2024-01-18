! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 20, 2005
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
!*  Procedure pointer component
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE ::DT
      INTEGER :: ID
      PROCEDURE(F1), PASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    INTERFACE
      FUNCTION F1(Arg)
      IMPORT DT
        CLASS(DT), INTENT(IN) :: Arg
        INTEGER      :: F1
      END FUNCTION

      FUNCTION F2(Arg)
      IMPORT DT
        CLASS(DT), INTENT(IN) :: Arg
        INTEGER      :: F2
      END FUNCTION
    END INTERFACE

  END MODULE

  FUNCTION F1(Arg)
  USE M, ONLY: DT
  CLASS(DT), INTENT(IN) :: Arg
  INTEGER      :: F1
    F1 = Arg%ID
  END FUNCTION

  FUNCTION F2(Arg)
  USE M, ONLY : DT
  CLASS(DT) :: Arg
  INTEGER      :: F2
    F2 = Arg%ID
  END FUNCTION

  PROGRAM PtrAssignProcNameTypBnd
  USE M
  IMPLICIT NONE

  TYPE (DT) :: V

  V%ProcPtr => F2
  V%ID = -1
  IF ( V%Procptr() .NE. -1 ) STOP 11

  END

