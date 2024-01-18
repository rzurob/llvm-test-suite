!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpCommon6
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
!*  The blank common blocks with different sizes
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
    INTEGER(K)    :: I(L)
    CHARACTER(L)  :: C2(L)
  END TYPE

  INTEGER, PARAMETER :: N1=20
  INTEGER, PARAMETER :: N2=40
  INTEGER, PARAMETER :: N =30

  CONTAINS

  SUBROUTINE Set1(I1, I2)

  INTEGER :: I1, I2, I
  TYPE(DT_I(2,7))  :: T
  COMMON T(N1)

  DO I=I1, I2
    T(I)%I  = I
    T(I)%C1=CHAR(I)
    T(I)%C2=CHAR(I)
  END DO

  END SUBROUTINE

  SUBROUTINE Set2(I1, I2)

  INTEGER :: I1, I2, I
  TYPE(DT_I(2,7))  :: T
  COMMON T(N2)

  DO I=I1, I2
    T(I)%I  = I
    T(I)%C1=CHAR(I)
    T(I)%C2=CHAR(I)
  END DO

  END SUBROUTINE

  END MODULE

  PROGRAM dtpCommon6
  USE M
  IMPLICIT NONE

  TYPE(DT_I(2,7))  :: T(N)

  COMMON T
  INTEGER I

  T=DT_I(2,7)(C1=CHAR(1), I=-1, C2=CHAR(2))

  CALL Set1(1, N1)

  DO I=1, N1
    IF ( ANY( T(I)%C1 .NE. CHAR(I) ) ) STOP 11
    IF ( ANY( T(I)%I  .NE. I       ) ) STOP 12
    IF ( ANY( T(I)%C2 .NE. CHAR(I) ) ) STOP 13
  END DO

  DO I=N1+1, N
    IF ( ANY( T(I)%C1 .NE. CHAR(1) ) ) STOP 21
    IF ( ANY( T(I)%I  .NE. -1      ) ) STOP 22
    IF ( ANY( T(I)%C2 .NE. CHAR(2) ) ) STOP 23
  END DO

  CALL Set2(N1+1, N2)

  DO I=1, N
    IF ( ANY( T(I)%C1 .NE. CHAR(I) ) ) STOP 31
    IF ( ANY( T(I)%I  .NE. I       ) ) STOP 32
    IF ( ANY( T(I)%C2 .NE. CHAR(I) ) ) STOP 33
  END DO


  END PROGRAM

