!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jul. 17, 2007
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
!*  A data pointer shall be storage associated only with data pointers of the same type and rank
!*
!*  (339347/339361)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT_I(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    CHARACTER(L)  :: C1(L)
    INTEGER(K)    :: I(L)
    CHARACTER(L)  :: C2(L)
  END TYPE

  TYPE(DT_I(2,7)), POINTER  :: Ptr0(:,:)
  COMMON Ptr0

  END MODULE


  PROGRAM dtpCommon8
  USE M, ONLY: Ptr0, DT_I
  IMPLICIT NONE

  TYPE(DT_I(2,7)), POINTER  :: Ptr(:,:)
  COMMON Ptr

  TYPE(DT_I(2,7)), TARGET  :: Tar(100)
!  COMMON Tar

  INTEGER I, J

  Ptr(0:9,0:9) => Tar

  DO I=0, 9
  DO J=0, 9
    Ptr(I,J)%I  = I*J
    Ptr(I,J)%C1=CHAR(I*J)
    Ptr(I,J)%C2=CHAR(I*J)
  END DO
  END DO

  IF ( ANY( LBOUND( Ptr0 ) .NE. [0, 0] ) ) STOP 11
  IF ( ANY( UBOUND( Ptr0 ) .NE. [9, 9] ) ) STOP 12

  DO I=0, 9
  DO J=0, 9
    IF ( ANY( Ptr0(I,J)%C1 .NE. CHAR(I*J) ) ) STOP 21
    IF ( ANY( Ptr0(I,J)%I  .NE. I*J       ) ) STOP 22
    IF ( ANY( Ptr0(I,J)%C2 .NE. CHAR(I*J) ) ) STOP 23
  END DO
  END DO

  END PROGRAM

