!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 06, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Default initialization for component
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
!*  The default initialization does not imply that the object has the SAVE attribute.
!*
!*  -- Dup of dtParamInitComp9.f to avoid ac imp do issue
!*
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE, ABSTRACT :: DT(K, L)
      INTEGER, KIND :: K=4
      INTEGER, LEN  :: L=4
    END TYPE

    INTEGER,        PARAMETER :: N(128)=[(I, I=1, 128)]

    TYPE, EXTENDS(DT) :: DT1(K1,L1)
      INTEGER(1), KIND :: K1=4
      INTEGER(1), LEN  :: L1=4
      INTEGER(K)   :: I(L)=N(K)
      REAL(K)      :: R(L1)=N(K)
    END TYPE

  END MODULE


  PROGRAM dtParamInitComp9_1
  USE M


  CALL IntSub()
  CALL IntSub()

  CALL ExtSub()
  CALL ExtSub()

  CONTAINS

  SUBROUTINE IntSub()
  TYPE(DT1) :: T

  IF ( T%K        .NE. 4 )                   STOP 11
  IF ( T%L        .NE. 4 )                   STOP 12
  IF ( T%K1       .NE. 4 )                   STOP 13
  IF ( T%L1       .NE. 4 )                   STOP 14

  IF ( KIND(T%I)   .NE. 4 )                STOP 21
  IF ( SIZE(T%I)   .NE. 4 )                STOP 22
  IF ( ANY(T%I     .NE. 4))                STOP 23

  IF ( KIND(T%R)   .NE. 4 )                STOP 31
  IF ( SIZE(T%R)   .NE. 4 )                STOP 32
  IF ( ANY(T%R     .NE. 4))                STOP 33

  T%I = -4
  T%R = -4.

  END SUBROUTINE

  END

  SUBROUTINE ExtSub()
  USE M
  TYPE(DT1(K=8,K1=8)), save :: T
  INTEGER, SAVE :: Count = 0


  IF ( T%K        .NE. 8 )                 STOP 11
  IF ( T%L        .NE. 4 )                 STOP 12
  IF ( T%K1       .NE. 8 )                 STOP 13
  IF ( T%L1       .NE. 4 )                 STOP 14

  IF ( KIND(T%I)   .NE. 8 )                STOP 21
  IF ( SIZE(T%I)   .NE. 4 )                STOP 22

  IF ( KIND(T%R)   .NE. 8 )                STOP 31
  IF ( SIZE(T%R)   .NE. 4 )                STOP 32

  IF (Count .EQ. 0 ) THEN
    IF ( ANY(T%I     .NE. 8))              STOP 41
    IF ( ANY(T%R     .NE. 8))              STOP 42
  ELSE
    IF ( ANY(T%I     .NE. -4))             STOP 51
    IF ( ANY(T%R     .NE. -4))             STOP 52
  END IF

  Count = 1
  T%I = -4
  T%R = -4.

  END SUBROUTINE

