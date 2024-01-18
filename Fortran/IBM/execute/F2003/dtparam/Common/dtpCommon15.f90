!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jul. 19, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration and specification
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  -- The common statement
!*
!*   A data object in a named common block may be initially defined by means of type declaration
!*   statement in a block data program unit
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(K,L1,L2)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L1=4
    INTEGER, LEN  :: L2=4
    SEQUENCE
    CHARACTER(L1)  :: C1(L2)
    INTEGER(K)     :: I(L2)
    CHARACTER(L2)  :: C2(L1)
  END TYPE

  CHARACTER(7), PARAMETER    :: C1(9) = "1234567"
  CHARACTER(9), PARAMETER    :: C2(7) = "987654321"
  INTEGER,      PARAMETER    :: K(9)  = [1,2,3,4,5,6,7,8,9]

  END MODULE

  BLOCK DATA
  USE M, ONLY: DT,C1,K,C2

  TYPE(DT(2,7,9))  :: T2(1024) = DT(2,7,9)(C1, K, C2)

  COMMON /B/ T2

  END BLOCK DATA

  PROGRAM dtpCommon15
  USE M, ONLY: DT,C1,K,C2
  IMPLICIT NONE

  TYPE(DT(2,7,9))  :: T2(1024)

  COMMON /B/ T2

  INTEGER         :: I

  DO I=1, 1024
    IF (  ANY ( T2(I)%C1 .NE. C1    ) ) STOP 24
    IF (  ANY ( T2(I)%I  .NE. K     ) ) STOP 25
    IF (  ANY ( T2(I)%C2 .NE. C2    ) ) STOP 26
  END DO

  END PROGRAM

