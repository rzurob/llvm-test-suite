!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamInitComp2_1
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
!*  -- Dup of dtParamInitComp9.f to avoid ac imp do issue
!*  (similar to 340286/341241 )
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    INTEGER, PARAMETER :: N(128)=[(I, I=1, 128)]
    INTEGER, PARAMETER :: ZN(128)=[((I,-I), I=1, 128)]

    TYPE, ABSTRACT :: DT(K, L)
      INTEGER, KIND :: K
      INTEGER, LEN  :: L
    END TYPE

    TYPE :: DT0(K0, L0)
      INTEGER, KIND :: K0
      INTEGER, LEN  :: L0
      INTEGER(K0)   :: I(K0)=K0
    END TYPE

    TYPE(DT0(4,1)), PARAMETER :: DTN(128)=DT0(4,1)()

    TYPE, EXTENDS(DT) :: DT1
      INTEGER(K)   :: I(K)=N(1:K)
      REAL(K)      :: R(K)=N(1:K)
      COMPLEX(K)   :: Z(K)=ZN(1:K)
      CHARACTER(L) :: C(L)="!!!!"
      PROCEDURE(IntFun), POINTER :: ProcPtr => NULL()
      LOGICAL(K)   :: LL(L)=.TRUE._1
      TYPE(DT0(K,L))  :: T(K)!=DTN(1:K)
    CONTAINS
      PROCEDURE, PASS :: IntFun
    END TYPE

  CONTAINS

    FUNCTION IntFun(Arg)
    CLASS(DT1(4,*)):: Arg
    TYPE(DT1(4, 1)):: IntFun
      IntFun = Arg
    END FUNCTION

  END MODULE


  PROGRAM dtParamInitComp2_1
  USE M

  TYPE(DT1(4,1))     :: T
  INTEGER, PARAMETER :: K=4
  INTEGER, PARAMETER :: L=1

  T =    DT1(4,1) (                        &
              I=(/(-T%K, I=1,T%K)/),            &
              R=(/(-T%K, I=1,T%K)/),            &
              Z=(/((-T%K,T%K), I=1,T%K)/),      &
              C="??????????",                   &
              ProcPtr=NULL(),                   &
              LL=(/(.FALSE._K, I=1,T%L)/),      &
              T=(/(DT0(T%K,T%L)((/(-T%K, J=1,T%K)/)), I=1,T%K)/) )


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

