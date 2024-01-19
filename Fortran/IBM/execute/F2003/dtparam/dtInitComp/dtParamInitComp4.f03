!*********************************************************************
!*  ===================================================================
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
!*  ()
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

    TYPE, EXTENDS(DT1) :: DT2
      INTEGER(K)   :: I(K)=K
      REAL(K)      :: R(K)=-K
      COMPLEX(K)   :: Z(K)=(K,-K)
      CHARACTER(L) :: C(L)="!!!!"
      PROCEDURE(IntFun), POINTER :: ProcPtr => NULL()
      LOGICAL(K)   :: LL(L)=.TRUE.
    CONTAINS
      PROCEDURE, PASS :: IntFun
    END TYPE

    TYPE(DT2(K=4, L=1)) :: T0
    SAVE                :: T0

  CONTAINS

    FUNCTION IntFun(Arg)
    CLASS(DT2(4,*,4,*)):: Arg
    TYPE(DT2(4, arg%l0, 4, arg%l)):: IntFun
      IntFun = Arg
    END FUNCTION

  END MODULE


  PROGRAM dtParamInitComp4
  USE M


  TYPE(DT2(L=1,K0=8,L0=-1))  :: T


  IF ( T%K0        .NE. 8 )                  ERROR STOP 11
  IF ( T%L0        .NE. -1 )                 ERROR STOP 12
  IF ( T%K         .NE. 8 )                  ERROR STOP 13
  IF ( T%L         .NE. 1 )                  ERROR STOP 14

  IF ( KIND(T%I)   .NE. 8 )                  ERROR STOP 15
  IF ( SIZE(T%I)   .NE. 8 )                  ERROR STOP 16
  IF ( ANY(T%I     .NE. (/(T%K, I=1,8)/)))  ERROR STOP 17

  IF ( KIND(T%R)   .NE. 8 )                  ERROR STOP 18
  IF ( SIZE(T%R)   .NE. 8 )                  ERROR STOP 19
  IF ( ANY(T%R     .NE. (/(-T%K, I=1,8)/)))  ERROR STOP 20

  IF ( KIND(T%Z)   .NE. 8 )                      ERROR STOP 21
  IF ( SIZE(T%Z)   .NE. 8 )                      ERROR STOP 22
  IF ( ANY(T%Z     .NE. (T%K,-T%K))) ERROR STOP 23

  IF ( LEN(T%C)    .NE. 1 )                ERROR STOP 24
  IF ( SIZE(T%C)   .NE. 1 )                ERROR STOP 25
  IF ( ANY(T%C     .NE. "!" ))      ERROR STOP 26

  IF ( ASSOCIATED(T%ProcPtr) )             ERROR STOP 27

  IF ( KIND(T%LL)  .NE. 8 )                ERROR STOP 28
  IF ( SIZE(T%LL)  .NE. 1 )                ERROR STOP 29
  IF ( ANY(T%LL    .NEQV. .true._8))      ERROR STOP 30



  IF ( T0%K0        .NE. 0 )                  ERROR STOP 31
  IF ( T0%L0        .NE. 0 )                  ERROR STOP 32
  IF ( T0%K         .NE. 4 )                  ERROR STOP 33
  IF ( T0%L         .NE. 1 )                  ERROR STOP 34

  IF ( KIND(T0%I)   .NE. 4 )                  ERROR STOP 35
  IF ( SIZE(T0%I)   .NE. 4 )                  ERROR STOP 36
  IF ( ANY(T0%I     .NE. T0%K))  ERROR STOP 37

  IF ( KIND(T0%R)   .NE. 4 )                  ERROR STOP 38
  IF ( SIZE(T0%R)   .NE. 4 )                  ERROR STOP 39
  IF ( ANY(T0%R     .NE. -T0%K))  ERROR STOP 40

  IF ( KIND(T0%Z)   .NE. 4 )                      ERROR STOP 41
  IF ( SIZE(T0%Z)   .NE. 4 )                      ERROR STOP 42
  IF ( ANY(T0%Z     .NE. (T0%K,-T0%K))) ERROR STOP 43

  IF ( LEN(T0%C)    .NE. 1 )                ERROR STOP 44
  IF ( SIZE(T0%C)   .NE. 1 )                ERROR STOP 45
  IF ( ANY(T0%C     .NE. "!" ))          ERROR STOP 46

  IF ( ASSOCIATED(T0%ProcPtr) )             ERROR STOP 47

  IF ( KIND(T0%LL)  .NE. 4 )                ERROR STOP 48
  IF ( SIZE(T0%LL)  .NE. 1 )                ERROR STOP 49
  IF ( ANY(T0%LL    .NEQV. .true._4))      ERROR STOP 50




  END

