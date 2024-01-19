!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jul. 13, 2007
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
!*  The storage sequence formed by data objects in the common block is extended to include
!*  all storage units of any storage sequence associated with it by equivalence association.
!*  The sequence may be extended only by adding storage units beyond the last storage unit.
!*  Data objects associated with an entity in a common block are considered to be in that common block
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT_I(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    CHARACTER(L)  :: C1(L)
    INTEGER(K)    :: I(L)!=K
    CHARACTER(L)  :: C2(L)
  END TYPE

  END MODULE

  PROGRAM dtpCommon5
  USE M
  IMPLICIT NONE

  TYPE(DT_I(2,7))  :: S(11), T(10)
  COMMON /BLK/S
  EQUIVALENCE (S(11), T(1))
  INTEGER I



  DO I=1, 11
    S(I)%I  = I
    S(I)%C1=CHAR(0)
    S(I)%C2=CHAR(0)
  END DO

  DO I=1, 10
    T(I)%I  = I+10
    T(I)%C1=CHAR(0)
    T(I)%C2=CHAR(0)
  END DO


  CALL ExtSub()

  END

  SUBROUTINE ExtSub()
  USE M

  TYPE(DT_I(2,7))  :: T
  COMMON /BLK/T(20)

  DO I=1, 10
    IF ( ANY( T(I)%C1 .NE. CHAR(0) ) ) ERROR STOP 11
    IF ( ANY( T(I)%I  .NE. I       ) ) ERROR STOP 12
    IF ( ANY( T(I)%C2 .NE. CHAR(0) ) ) ERROR STOP 13
  END DO

  END

