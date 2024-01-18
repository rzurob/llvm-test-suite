!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamInitComp7_1
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
!*  If the component is of a type for which default initialization is specified for a component,
!*  the default initialization specified by initialization-expr overrides the default initialization
!*  specified for that component.
!*
!*  (341246)
!*  -- Dup of dtParamInitComp9.f to avoid ac imp do issue
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

   TYPE :: DT2(K2,L2)
     INTEGER, KIND :: K2=0
     INTEGER, LEN  :: L2=0

     TYPE(DT1(K=K2, L=L2)) :: T=DT1(K=K2, L=8)  ( &
               I=(/(-K2, I=1,K2)/),               &
               R=(/(-K2, I=1,K2)/),               &
               Z=(/((-K2,K2), I=1,K2)/),          &
               C="??????????",                    &
               LL=(/(.FALSE._4, I=1,K2)/)   )
    END TYPE

  END MODULE


  PROGRAM dtParamInitComp7_1
  USE M

  TYPE(DT2(K2=8, L2=8)) :: T2


  IF ( T2%T%K       .NE. 8 )                 STOP 41
  IF ( T2%T%L       .NE. 8 )                 STOP 42
  IF ( T2%K2        .NE. 8 )                 STOP 43
  IF ( T2%L2        .NE. 8 )                 STOP 44

  IF ( KIND(T2%T%I) .NE. 8 )                 STOP 45
  IF ( SIZE(T2%T%I) .NE. 8 )                 STOP 46
  IF ( ANY(T2%T%I   .NE. -8))                STOP 47

  IF ( KIND(T2%T%R) .NE. 8 )                 STOP 48
  IF ( SIZE(T2%T%R) .NE. 8 )                 STOP 49
  IF ( ANY(T2%T%R   .NE. -8))                STOP 40

  IF ( KIND(T2%T%Z) .NE. 8 )                 STOP 51
  IF ( SIZE(T2%T%Z) .NE. 8 )                 STOP 52
  IF ( ANY(T2%T%Z   .NE. (-8,8)))            STOP 53

  IF ( LEN(T2%T%C)  .NE. 8 )                 STOP 54
  IF ( SIZE(T2%T%C) .NE. 8 )                 STOP 55
  IF ( ANY(T2%T%C   .NE. "????????" ))       STOP 56


  END

