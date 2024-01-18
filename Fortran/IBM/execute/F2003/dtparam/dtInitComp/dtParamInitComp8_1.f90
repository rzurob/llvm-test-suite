!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamInitComp8_1 
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : June 06, 2006
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
!*  Explicit initialization in a type declaration statement overrides default initialization  
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
    INTEGER,        PARAMETER :: ZN(128)=[((I,-I), I=1, 128)]

    TYPE, EXTENDS(DT) :: DT1
      INTEGER(K)   :: I(L)=N(1:K)
      REAL(K)      :: R(L)=N(1:K)
      COMPLEX(K)   :: Z(L)=ZN(1:K)
      CHARACTER(L) :: C(L)="!!!!"
      LOGICAL(K)   :: LL(L)=.TRUE._8
    END TYPE

    TYPE, extends(dt1) :: DT2(K2,L2)
      INTEGER, KIND :: K2=0
      INTEGER, LEN  :: L2=0

      TYPE(DT1(K=K2, L=L2)) :: T=DT1(K=K2, L=4) ( &
                I=-K2,               &
                R=-K2,               &
                Z=(-K2,K2),          &
                C="??????????",      &
                LL=.FALSE._4         )    
    END TYPE


  END MODULE


  PROGRAM dtParamInitComp8_1 
  USE M

  TYPE(DT2(K2=4, L2=4)) :: T=DT2(K2=4, L2=4) ( &
            I=3,                &
            R=3,                &
            Z=(-3,-3),          &
            C="$$$$$$",         &
            LL=.TRUE._4   )


  IF ( T%T%K       .NE. 4 )                 STOP 41
  IF ( T%T%L       .NE. 4 )                 STOP 42
  IF ( T%K2        .NE. 4 )                 STOP 43
  IF ( T%L2        .NE. 4 )                 STOP 44

  IF ( KIND(T%I)   .NE. 4 )                 STOP 45
  IF ( SIZE(T%I)   .NE. 4 )                 STOP 46
  IF ( ANY(T%I     .NE. 3))                 STOP 47

  IF ( KIND(T%R)   .NE. 4 )                 STOP 48
  IF ( SIZE(T%R)   .NE. 4 )                 STOP 49
  IF ( ANY(T%R     .NE. 3))                 STOP 40

  IF ( KIND(T%Z)   .NE. 4 )                 STOP 51
  IF ( SIZE(T%Z)   .NE. 4 )                 STOP 52
  IF ( ANY(T%Z     .NE. (-3,-3)))           STOP 53

  IF ( LEN(T%C)    .NE. 4 )                 STOP 54
  IF ( SIZE(T%C)   .NE. 4 )                 STOP 55
  IF ( ANY(T%C     .NE. "$$$$" ))           STOP 56


  END

