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
!*  An object with the TARGET attribute may be storage associated only with another object
!*  that has the TARGET attribute and the same type and type parameters
!*
!*  -- the target attribute
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

  TYPE(DT(2,:,:)), POINTER :: Ptr(:)
  TYPE(DT(2,7,9)),  TARGET  :: T1(1024)

  COMMON T1, Ptr

  END MODULE


  PROGRAM dtpCommon13
  USE M
  IMPLICIT NONE

  TYPE(DT(2,7,9)), TARGET :: T(1024)
  CHARACTER(7)    :: C1(9)
  CHARACTER(9)    :: C2(7)
  INTEGER         :: K(9)
  INTEGER         :: I
  COMMON T

  Ptr => T

  C1 = "123456789"
  K = [1,2,3,4,5,6,7,8,9]
  C2 = "7654321"

  DO I=1, 1024
    T(I)%C1 = C1
    T(I)%I  = K
    T(I)%C2 = C2
  END DO

  IF ( .NOT. ASSOCIATED(Ptr, T ) ) STOP 11
  IF ( .NOT. ASSOCIATED(Ptr, T1) ) STOP 12

  DO I=1, 1024
    IF (  ANY ( T1(I)%C1 .NE. C1    ) ) STOP 21
    IF (  ANY ( T1(I)%I  .NE. K     ) ) STOP 22
    IF (  ANY ( T1(I)%C2 .NE. C2    ) ) STOP 23
  END DO

  END PROGRAM

