!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpCommon3
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
!*  The demension attribute specified by the common stmt
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT_R(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    REAL(K)       :: R(L)=K
  END TYPE

  TYPE :: DT_C(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    CHARACTER(L)  :: C(L)=CHAR(48+K)
  END TYPE

  TYPE :: DT_I(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    INTEGER(K)    :: I(L)=K
  END TYPE

  TYPE :: DT_L(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    LOGICAL(K)    :: A(L)=.TRUE.
  END TYPE

  TYPE :: DT_Z(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    COMPLEX(K)    :: Z(L)=(K,-K)
  END TYPE

  TYPE :: DT(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    TYPE(DT_R(K,L))  :: R
    TYPE(DT_C(K,L))  :: C
    TYPE(DT_I(K,L))  :: I
    TYPE(DT_L(K,L))  :: A
    TYPE(DT_Z(K,L))  :: Z
  END TYPE

  END MODULE

  PROGRAM dtpCommon3
  USE M
  IMPLICIT NONE

  INTEGER, PARAMETER :: N=128
  TYPE(DT(8,7))  :: T
  COMMON /BLK/T(N)

  INTEGER I

  CALL ExtSub()

  DO I=1, N

    IF ( SIZE(T(I)%R%R) .NE. 7   ) STOP 21
    IF ( ANY (T(I)%R%R  .NE. 8 ) ) STOP 22

    IF ( SIZE(T(I)%C%C) .NE. 7         ) STOP 31
    IF ( ANY (T(I)%C%C  .NE. CHAR(8) ) ) STOP 32

    IF ( SIZE(T(I)%I%I) .NE. 7   ) STOP 41
    IF ( ANY (T(I)%I%I  .NE. 8 ) ) STOP 42

    IF ( SIZE(T(I)%A%A) .NE.   7        ) STOP 51
    IF ( ANY (T(I)%A%A  .NEQV. .TRUE. ) ) STOP 52

    IF ( SIZE(T(I)%Z%Z) .NE. 7        ) STOP 61
    IF ( ANY (T(I)%Z%Z  .NE. (8,-8) ) ) STOP 62

  END DO

  END

  SUBROUTINE ExtSub()
  USE M

  INTEGER, PARAMETER :: N1=100, N2=28

  TYPE(DT(8,7))  :: T1, T2
  COMMON /BLK/T1(N1), T2(N2)

  DO I=1, N1
    T1(I)%R%R = 8
    T1(I)%C%C = CHAR(8)
    T1(I)%I%I = 8
    T1(I)%A%A = .TRUE.
    T1(I)%Z%Z = (8,-8)
  END DO

  DO I=1, N2
    T2(I)%R%R = 8
    T2(I)%C%C = CHAR(8)
    T2(I)%I%I = 8
    T2(I)%A%A = .TRUE.
    T2(I)%Z%Z = (8,-8)
  END DO

  END


