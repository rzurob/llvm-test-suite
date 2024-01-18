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
!*  Explicit initialization in a type declaration statement overrides default initialization
!*
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  !integer, parameter :: K=4
  !integer, parameter :: L=0

    TYPE, ABSTRACT :: DT(K, L)
      INTEGER, KIND :: K=4
      INTEGER, LEN  :: L=4
    END TYPE

    TYPE, EXTENDS(DT) :: DT1
      INTEGER(K)   :: I(L)=K
      REAL(K)      :: R(L)=K
      COMPLEX(K)   :: Z(L)=(K,-K)
      CHARACTER(L) :: C(L)="!!!!"
      PROCEDURE(IntFun), POINTER :: ProcPtr => NULL()
      LOGICAL(K)   :: LL(L)=.TRUE._8
    CONTAINS
      PROCEDURE, PASS :: IntFun
    END TYPE

    TYPE :: DT2(K2,L2)
      INTEGER, KIND :: K2=0
      INTEGER, LEN  :: L2=0

      TYPE(DT1(K=K2, L=k2)) :: T=DT1(K=K2, L=k2) ( &
                I=(/(-K2, I=1,K2)/),               &
                R=(/(-K2, I=1,K2)/),               &
                Z=(/((-K2,K2), I=1,K2)/),          &
                C="??????????",                    &
                ProcPtr=NULL(),                    &
                LL=(/(.FALSE._4, I=1,K2)/)   )
    END TYPE

  CONTAINS

    FUNCTION IntFun(Arg)
    CLASS(DT1(4,*)):: Arg
    TYPE(DT1(4, arg%l)):: IntFun
      IntFun = Arg
    END FUNCTION

  END MODULE


  PROGRAM dtParamInitComp8
  USE M

  TYPE(DT2(K2=4, L2=4)) :: T=DT2(K2=4, L2=4) ( &
         t=dt1(4,4) ( &
            I=3,                &
            R=3,                &
            Z=(-3,-3),          &
            C="$$$$$$",         &
            ProcPtr=NULL(),     &
            LL=.TRUE._4   ))


  IF ( T%T%K      .NE. 4 )                 STOP 41
  IF ( T%T%L      .NE. 4 )                 STOP 42
  IF ( T%K2        .NE. 4 )                 STOP 43
  IF ( T%L2        .NE. 4 )                 STOP 44

  IF ( KIND(T%t%I)   .NE. 4 )                 STOP 45
  IF ( SIZE(T%t%I)   .NE. 4 )                 STOP 46
  IF ( ANY(T%t%I     .NE. 3))                 STOP 47

  IF ( KIND(T%t%R)   .NE. 4 )                 STOP 48
  IF ( SIZE(T%t%R)   .NE. 4 )                 STOP 49
  IF ( ANY(T%t%R     .NE. 3))                 STOP 40

  IF ( KIND(T%t%Z)   .NE. 4 )                 STOP 51
  IF ( SIZE(T%t%Z)   .NE. 4 )                 STOP 52
  IF ( ANY(T%t%Z     .NE. (-3,-3)))           STOP 53

  IF ( LEN(T%t%C)    .NE. 4 )                 STOP 54
  IF ( SIZE(T%t%C)   .NE. 4 )                 STOP 55
  IF ( ANY(T%t%C     .NE. "$$$$" ))           STOP 56

  IF ( ASSOCIATED(T%t%ProcPtr) )              STOP 57



  END

