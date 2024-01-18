!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpCommon14
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
!*     Execution of a RETURN or END statement may cause data objects in a named common
!*     block to become undefined unless the common block name has been declared in a SAVE
!*     statement, but never causes data objects in blank common to become undefined
!*
!*  (339432)
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

  CHARACTER(7)    :: C1(9) = "1234567"
  CHARACTER(9)    :: C2(7) = "987654321"
  INTEGER         :: K(9)  = [1,2,3,4,5,6,7,8,9]
  COMMON T1, Ptr

  END MODULE

  SUBROUTINE ExtSub()
  USE M, ONLY: DT,C1,K,C2

  TYPE(DT(2,:,:)), POINTER  :: Ptr1(:), Ptr2(:)
  TYPE(DT(2,7,9)), TARGET   :: T1(1024), T2(1024)
  !SAVE T2, Ptr2
  SAVE /B/
  COMMON T1, Ptr1
  COMMON /B/ T2, Ptr2

  INTEGER         :: I

  Ptr1 => T1
  Ptr2 => T2

  DO I=1, 1024
    T1(I)%C1 = C1
    T1(I)%I  = K
    T1(I)%C2 = C2

    T2(I)%C1 = C1
    T2(I)%I  = K
    T2(I)%C2 = C2
  END DO

  END SUBROUTINE

  PROGRAM dtpCommon14
  USE M, ONLY: DT,C1,K,C2
  IMPLICIT NONE

  TYPE(DT(2,:,:)), POINTER  :: Ptr1(:), Ptr2(:)
  TYPE(DT(2,7,9)), TARGET   :: T1(1024), T2(1024)
  COMMON T1, Ptr1
  COMMON /B/ T2, Ptr2
  INTEGER         :: I

  CALL ExtSub()

  IF ( .NOT. ASSOCIATED( Ptr1, T1 ) ) STOP 31
  IF ( .NOT. ASSOCIATED( Ptr2, T2 ) ) STOP 31

  DO I=1, 1024
    IF (  ANY ( Ptr1(I)%C1 .NE. C1    ) ) STOP 21
    IF (  ANY ( Ptr1(I)%I  .NE. K     ) ) STOP 22
    IF (  ANY ( Ptr1(I)%C2 .NE. C2    ) ) STOP 23

    IF (  ANY ( Ptr2(I)%C1 .NE. C1    ) ) STOP 24
    IF (  ANY ( Ptr2(I)%I  .NE. K     ) ) STOP 25
    IF (  ANY ( Ptr2(I)%C2 .NE. C2    ) ) STOP 26
  END DO

  END PROGRAM

