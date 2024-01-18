!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamInitComp4_1
!*
!*  DATE                       : May. 29, 2006
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
!*  Entities with the save in module/main
!*
!*  -- Dup of dtParamInitComp9.f to avoid ac imp do issue
!*  (similar to 324069)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE, ABSTRACT :: DT0(K0, L0)
      INTEGER, KIND :: K0=0
      INTEGER, LEN  :: L0=0
    END TYPE

    TYPE, EXTENDS(DT0) :: DT1(K, L)
      INTEGER, KIND :: K=K0
      INTEGER, LEN  :: L=0
    END TYPE

    INTEGER,        PARAMETER :: N(128)=[(I, I=1, 128)]
    INTEGER,        PARAMETER :: ZN(128)=[((I,-I), I=1, 128)]

    TYPE, EXTENDS(DT1) :: DT2
      INTEGER(K)   :: I(K)=N(1:K)
      REAL(K)      :: R(K)=N(1:K)
      COMPLEX(K)   :: Z(K)=ZN(1:K)
      CHARACTER(L) :: C(L)="!"
      LOGICAL(K)   :: LL(L)=.TRUE._8
    END TYPE

    TYPE(DT2(K=4, L=1)) :: T0
    SAVE                :: T0

  END MODULE


  PROGRAM dtParamInitComp4_1
  USE M


  TYPE(DT2(L=1,K0=8,L0=-1))  :: T


  IF ( T%K0        .NE. 8 )                  STOP 11
  IF ( T%L0        .NE. -1 )                 STOP 12
  IF ( T%K         .NE. 8 )                  STOP 13
  IF ( T%L         .NE. 1 )                  STOP 14

  IF ( KIND(T%I)   .NE. 8 )                  STOP 15
  IF ( SIZE(T%I)   .NE. 8 )                  STOP 16
  IF ( ANY(T%I     .NE. (/(i, I=1,8)/)))  STOP 17

  IF ( KIND(T%R)   .NE. 8 )                  STOP 18
  IF ( SIZE(T%R)   .NE. 8 )                  STOP 19
  IF ( ANY(T%R     .NE. (/(n(i), I=1,8)/)))  STOP 20

  IF ( KIND(T%Z)   .NE. 8 )                      STOP 21
  IF ( SIZE(T%Z)   .NE. 8 )                      STOP 22
  IF ( ANY(T%Z     .NE. (/(zn(i),I=1,8)/))) STOP 23

  IF ( LEN(T%C)    .NE. 1 )                STOP 24
  IF ( SIZE(T%C)   .NE. 1 )                STOP 25
  IF ( ANY(T%C     .NE. "!" ))             STOP 26


  IF ( KIND(T%LL)  .NE. 8 )                STOP 28
  IF ( SIZE(T%LL)  .NE. 1 )                STOP 29
  IF ( ANY(T%LL    .NEQV. .true.))      STOP 30



  IF ( T0%K0        .NE. 0 )                  STOP 31
  IF ( T0%L0        .NE. 0 )                  STOP 32
  IF ( T0%K         .NE. 4 )                  STOP 33
  IF ( T0%L         .NE. 1 )                  STOP 34

  IF ( KIND(T0%I)   .NE. 4 )                  STOP 35
  IF ( SIZE(T0%I)   .NE. 4 )                  STOP 36
  IF ( ANY(T0%I     .NE. (/(n(i), I=1,4)/)))  STOP 37

  IF ( KIND(T0%R)   .NE. 4 )                  STOP 38
  IF ( SIZE(T0%R)   .NE. 4 )                  STOP 39
  IF ( ANY(T0%R     .NE. (/(n(i), I=1,4)/)))  STOP 40

  IF ( KIND(T0%Z)   .NE. 4 )                      STOP 41
  IF ( SIZE(T0%Z)   .NE. 4 )                      STOP 42
  IF ( ANY(T0%Z     .NE. (/(zn(i),I=1,4)/))) STOP 43

  IF ( LEN(T0%C)    .NE. 1 )                STOP 44
  IF ( SIZE(T0%C)   .NE. 1 )                STOP 45
  IF ( ANY(T0%C     .NE. "!" ))             STOP 46



  END

