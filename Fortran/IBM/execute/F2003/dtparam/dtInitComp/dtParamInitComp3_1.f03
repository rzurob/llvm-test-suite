!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 26, 2006
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
!*  Initialized by type declaration statement
!*
!*  -- Dup of dtParamInitComp9.f to avoid ac imp do issue
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE, ABSTRACT :: DT0(K0, L0)
      INTEGER, KIND :: K0=0
      INTEGER, LEN  :: L0=0
    END TYPE

    TYPE, EXTENDS(DT0) :: DT1(K, L)
      INTEGER, KIND :: K=4
      INTEGER, LEN  :: L=1
    END TYPE

    INTEGER,        PARAMETER :: N(128)=[(I, I=1, 128)]
    INTEGER,        PARAMETER :: ZN(128)=[((I,-I), I=1, 128)]
    TYPE(DT1(K=4,L=1)), PARAMETER :: DTN(128)=DT1(K=4,L=1)()

    TYPE, EXTENDS(DT1) :: DT2
      INTEGER(K)   :: I(K)=N(1:K)
      REAL(K)      :: R(K)=N(1:K)
      COMPLEX(K)   :: Z(K)=ZN(1:K)
      CHARACTER(L) :: C(L)="!!!!"
      PROCEDURE(IntFun), POINTER :: ProcPtr => NULL()
      LOGICAL(K)   :: LL(L)=.TRUE._1
      TYPE(DT1(K,L))  :: T(K)
    CONTAINS
      PROCEDURE, PASS :: IntFun
    END TYPE

  CONTAINS

    FUNCTION IntFun(Arg)
    CLASS(DT2(0,K=4,L0=*,L=*)):: Arg
    TYPE(DT2(0,0,4, L=1)):: IntFun
!      IntFun = Arg
    END FUNCTION

  END MODULE


  PROGRAM dtParamInitComp3_1
  USE M

  INTEGER, PARAMETER :: K=4
  INTEGER, PARAMETER :: L=1

  TYPE(DT1(K=4, L=1))  :: T1=DT1(K=4, L=1)()
  TYPE(DT1(K=4, L=1))  :: T2=DT1(K=4, L=1)()
  TYPE(DT1(K=4, L=1))  :: T3=DT1(K=4, L=1, K0=0)()
  TYPE(DT1(K0=0, L0=0)) :: T4=DT1(K0=0, L0=0)()


  TYPE(DT2(K=4,L=1,K0=1,L0=1))     :: T  =    &
              DT2(1,1,4,1) (                        &
              I=(/(-T%K, I=1,T%K)/),            &
              R=(/(-T%K, I=1,T%K)/),            &
              Z=(/((-T%K,T%K), I=1,T%K)/),      &
              C="??????????",                   &
              ProcPtr=NULL(),                   &
              LL=(/(.FALSE._K, I=1,T%L)/), t = dt1(4,1,4,1)() )


  IF ( T%K0        .NE. 1 )                 ERROR STOP 41
  IF ( T%L0        .NE. 1 )                 ERROR STOP 42
  IF ( T%K         .NE. 4 )                  ERROR STOP 43
  IF ( T%L         .NE. 1 )                  ERROR STOP 44

  IF ( KIND(T%I)   .NE. K )                  ERROR STOP 45
  IF ( SIZE(T%I)   .NE. K )                  ERROR STOP 46
  IF ( ANY(T%I     .NE. (/(-T%K, I=1,4)/)))  ERROR STOP 47

  IF ( KIND(T%R)   .NE. K )                  ERROR STOP 48
  IF ( SIZE(T%R)   .NE. K )                  ERROR STOP 49
  IF ( ANY(T%R     .NE. (/(-T%K, I=1,4)/)))  ERROR STOP 40

  IF ( KIND(T%Z)   .NE. K )                      ERROR STOP 51
  IF ( SIZE(T%Z)   .NE. K )                      ERROR STOP 52
  IF ( ANY(T%Z     .NE. (/((-T%K,T%K),I=1,4)/))) ERROR STOP 53

  IF ( LEN(T%C)    .NE. L )                ERROR STOP 54
  IF ( SIZE(T%C)   .NE. L )                ERROR STOP 55
  IF ( ANY(T%C     .NE. "?" ))             ERROR STOP 56

  IF ( ASSOCIATED(T%ProcPtr) )             ERROR STOP 57

  IF ( KIND(T%LL)  .NE. K )                ERROR STOP 58
  IF ( SIZE(T%LL)  .NE. K )                ERROR STOP 59
  IF ( ANY(T%LL    .NEQV. .FALSE._K))      ERROR STOP 50

  IF ( T%T%K0      .NE. 1 )               ERROR STOP 61
  IF ( T%T%L0      .NE. 1 )               ERROR STOP 62
  IF ( T%T%K       .NE. 4 )                ERROR STOP 63
  IF ( T%T%L       .NE. 1 )                ERROR STOP 64


  IF ( T1%K0 .NE. 0 )                 ERROR STOP 11
  IF ( T1%L0 .NE. 0 )                 ERROR STOP 12
  IF ( T1%K  .NE. K )                 ERROR STOP 13
  IF ( T1%L  .NE. L )                 ERROR STOP 14

  IF ( T3%K0 .NE. 0 )                 ERROR STOP 21
  IF ( T3%L0 .NE. 0 )                 ERROR STOP 22
  IF ( T3%K  .NE. K )                 ERROR STOP 23
  IF ( T3%L  .NE. L )                 ERROR STOP 24

  IF ( T4%K0 .NE. 0 )                 ERROR STOP 31
  IF ( T4%L0 .NE. 0 )                 ERROR STOP 32
  IF ( T4%K  .NE. 4 )                 ERROR STOP 33
  IF ( T4%L  .NE. 1 )                 ERROR STOP 34


  END

