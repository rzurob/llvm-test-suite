!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamInitComp2
!*
!*  DATE                       : May. 25, 2006
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
!* Initialization with data stmt
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE, ABSTRACT :: DT(K, L)
      INTEGER, KIND :: K
      INTEGER, LEN  :: L
    END TYPE

    TYPE :: DT0(K0, L0)
      INTEGER, KIND :: K0
      INTEGER, LEN  :: L0
      INTEGER(K0)   :: I(K0)!=K0
    END TYPE

    TYPE, EXTENDS(DT) :: DT1
      INTEGER(K)   :: I(K)!=K
      REAL(K)      :: R(K)!=K
      COMPLEX(K)   :: Z(K)!=(K,-K)
      CHARACTER(L) :: C(L)!="!!!!"
      PROCEDURE(IntFun), POINTER :: ProcPtr! => NULL()
      LOGICAL(K)   :: LL(L)!=.TRUE.
      TYPE(DT0(K,L))  :: T(K)
    CONTAINS
      PROCEDURE, PASS :: IntFun
    END TYPE

  CONTAINS

    FUNCTION IntFun(Arg)
    CLASS(DT1(4,*)):: Arg
    TYPE(DT1(4, arg%l)):: IntFun
      IntFun = Arg
    END FUNCTION

  END MODULE


  PROGRAM dtParamInitComp2
  USE M

  INTEGER, PARAMETER :: K=4
  INTEGER, PARAMETER :: L=1

  TYPE(DT1(4,1))     :: T
  DATA T /    DT1(4,1) (                        &
              I=(/(-T%K, I=1,T%K)/),            &
              R=(/(-T%K, I=1,T%K)/),            &
              Z=(/((-T%K,T%K), I=1,T%K)/),      &
              C="??????????",                   &
              ProcPtr=NULL(),                   &
              LL=(/(.FALSE., I=1,T%L)/),      &
              T=DT0(T%K,T%L)(-T%K) ) &
         /


  IF ( KIND(T%I) .NE. K )                 STOP 11
  IF ( SIZE(T%I) .NE. K )                 STOP 12
  IF ( ANY(T%I  .NE. (/(-T%K, I=1,4)/)))  STOP 13

  IF ( KIND(T%R) .NE. K )                 STOP 14
  IF ( SIZE(T%R) .NE. K )                 STOP 16
  IF ( ANY(T%R  .NE. (/(-T%K, I=1,4)/)))  STOP 17

  IF ( KIND(T%Z) .NE. K )                 STOP 18
  IF ( SIZE(T%Z) .NE. K )                 STOP 19
  IF ( ANY(T%Z  .NE. (/((-T%K,T%K),I=1,4)/))) STOP 20

  IF ( LEN(T%C) .NE. L )                    STOP 21
  IF ( SIZE(T%C).NE. L )                    STOP 22
  IF ( ANY(T%C  .NE. "?" ))                 STOP 23

  IF ( ASSOCIATED(T%ProcPtr) )              STOP 24

  IF ( KIND(T%LL) .NE. K )                  STOP 25
  IF ( SIZE(T%LL) .NE. l )                  STOP 26
  IF ( ANY(T%LL  .NEQV. .FALSE._K))         STOP 27

  IF ( T%T%K0 .NE. K )                      STOP 29
  IF ( T%T%L0 .NE. L )                      STOP 31
  IF ( SIZE(T%T) .NE. K )                   STOP 32
  IF ( SIZE(T%T(1)%I) .NE. K )                 STOP 33
  IF ( ANY(T%T(1)%I  .NE. -K ))              STOP 34


  END

