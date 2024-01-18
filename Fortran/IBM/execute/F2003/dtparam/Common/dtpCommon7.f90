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
!*  Use association
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M0

  TYPE :: DT_I(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    CHARACTER(L)  :: C1(L)
    INTEGER(K)    :: I(L)
    CHARACTER(L)  :: C2(L)
  END TYPE

  INTEGER, PARAMETER :: N =3000

  TYPE(DT_I(2,7))  :: T0
  COMMON T0(N)

  END MODULE


  MODULE M1
  USE M0, ONLY: N, DT_I, T0

  TYPE(DT_I(2,7))  :: T1
  COMMON T1(N)

  END MODULE


  PROGRAM dtpCommon7
! USE M, ONLY : DT_I, N
  USE M1
  IMPLICIT NONE

  TYPE(DT_I(2,7))  :: T(N)
  COMMON T

  INTEGER I

  DO I=1, N
    T0(I)%I  = I
    T0(I)%C1=CHAR(I)
    T0(I)%C2=CHAR(I)
  END DO

  DO I=1, N
    IF ( ANY( T(I)%C1 .NE. CHAR(I) ) ) STOP 11
    IF ( ANY( T(I)%I  .NE. I       ) ) STOP 12
    IF ( ANY( T(I)%C2 .NE. CHAR(I) ) ) STOP 13
  END DO

  DO I=1, N
    IF ( ANY( T1(I)%C1 .NE. CHAR(I) ) ) STOP 21
    IF ( ANY( T1(I)%I  .NE. I       ) ) STOP 22
    IF ( ANY( T1(I)%C2 .NE. CHAR(I) ) ) STOP 23
  END DO


  DO I=1, N
    T0(I)%I  = I-1
    T0(I)%C1=CHAR(I-1)
    T0(I)%C2=CHAR(I-1)
  END DO


  DO I=1, N
    IF ( ANY( T0(I)%C1 .NE. CHAR(I-1) ) ) STOP 31
    IF ( ANY( T0(I)%I  .NE. I-1       ) ) STOP 32
    IF ( ANY( T0(I)%C2 .NE. CHAR(I-1) ) ) STOP 33
  END DO

  DO I=1, N
    IF ( ANY( T1(I)%C1 .NE. CHAR(I-1) ) ) STOP 41
    IF ( ANY( T1(I)%I  .NE. I-1       ) ) STOP 42
    IF ( ANY( T1(I)%C2 .NE. CHAR(I-1) ) ) STOP 43
  END DO


  END PROGRAM

