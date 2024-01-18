! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/CrossFeatures2/PtrAssignProcNameTypBnd.f
! opt variations: -ql

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

    TYPE ::DT(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: ID
      PROCEDURE(F1), PASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    INTERFACE
      FUNCTION F1(Arg)
      IMPORT DT
        CLASS(DT(4)), INTENT(IN) :: Arg
        INTEGER      :: F1
      END FUNCTION

      FUNCTION F2(Arg)
      IMPORT DT
        CLASS(DT(4)), INTENT(IN) :: Arg
        INTEGER      :: F2
      END FUNCTION
    END INTERFACE

  END MODULE

  FUNCTION F1(Arg)
  USE M, ONLY: DT
  CLASS(DT(4)), INTENT(IN) :: Arg
  INTEGER      :: F1
    F1 = Arg%ID
  END FUNCTION

  FUNCTION F2(Arg)
  USE M, ONLY : DT
  CLASS(DT(4)) :: Arg
  INTEGER      :: F2
    F2 = Arg%ID
  END FUNCTION

  PROGRAM PtrAssignProcNameTypBnd
  USE M
  IMPLICIT NONE

  TYPE (DT(4)) :: V

  V%ProcPtr => F2
  V%ID = -1
  IF ( V%Procptr() .NE. -1 ) STOP 11

  END

