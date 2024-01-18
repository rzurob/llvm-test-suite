!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamInitComp3_1 
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 26, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Default initialization for component 
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
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


  IF ( T%K0        .NE. 1 )                 STOP 41
  IF ( T%L0        .NE. 1 )                 STOP 42
  IF ( T%K         .NE. 4 )                  STOP 43
  IF ( T%L         .NE. 1 )                  STOP 44

  IF ( KIND(T%I)   .NE. K )                  STOP 45
  IF ( SIZE(T%I)   .NE. K )                  STOP 46
  IF ( ANY(T%I     .NE. (/(-T%K, I=1,4)/)))  STOP 47

  IF ( KIND(T%R)   .NE. K )                  STOP 48
  IF ( SIZE(T%R)   .NE. K )                  STOP 49
  IF ( ANY(T%R     .NE. (/(-T%K, I=1,4)/)))  STOP 40

  IF ( KIND(T%Z)   .NE. K )                      STOP 51
  IF ( SIZE(T%Z)   .NE. K )                      STOP 52
  IF ( ANY(T%Z     .NE. (/((-T%K,T%K),I=1,4)/))) STOP 53

  IF ( LEN(T%C)    .NE. L )                STOP 54
  IF ( SIZE(T%C)   .NE. L )                STOP 55
  IF ( ANY(T%C     .NE. "?" ))             STOP 56

  IF ( ASSOCIATED(T%ProcPtr) )             STOP 57

  IF ( KIND(T%LL)  .NE. K )                STOP 58
  IF ( SIZE(T%LL)  .NE. K )                STOP 59
  IF ( ANY(T%LL    .NEQV. .FALSE._K))      STOP 50

  IF ( T%T%K0      .NE. 1 )               STOP 61
  IF ( T%T%L0      .NE. 1 )               STOP 62
  IF ( T%T%K       .NE. 4 )                STOP 63
  IF ( T%T%L       .NE. 1 )                STOP 64


  IF ( T1%K0 .NE. 0 )                 STOP 11
  IF ( T1%L0 .NE. 0 )                 STOP 12
  IF ( T1%K  .NE. K )                 STOP 13
  IF ( T1%L  .NE. L )                 STOP 14
 
  IF ( T3%K0 .NE. 0 )                 STOP 21
  IF ( T3%L0 .NE. 0 )                 STOP 22
  IF ( T3%K  .NE. K )                 STOP 23
  IF ( T3%L  .NE. L )                 STOP 24
 
  IF ( T4%K0 .NE. 0 )                 STOP 31
  IF ( T4%L0 .NE. 0 )                 STOP 32
  IF ( T4%K  .NE. 4 )                 STOP 33
  IF ( T4%L  .NE. 1 )                 STOP 34
 

  END

