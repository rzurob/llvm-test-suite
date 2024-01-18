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
!*  If the component is of a type for which default initialization is specified for a component,
!*  the default initialization specified by initialization-expr overrides the default initialization
!*  specified for that component.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

!  integer, parameter :: K=4
!  integer, parameter :: L=0

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

     TYPE(DT1(K=K2, L=K2)) :: T=DT1(K=K2, L=K2) ( &
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


  PROGRAM dtParamInitComp7
  USE M

  TYPE(DT2(K2=4, L2=0)) :: T1
  TYPE(DT2(K2=8, L2=8)) :: T2

  IF ( T1%T%K      .NE. 4 )                 ERROR STOP 21
  IF ( T1%T%L      .NE. 4 )                 ERROR STOP 22
  IF ( T1%K2        .NE. 4 )                 ERROR STOP 23
  IF ( T1%L2        .NE. 0 )                 ERROR STOP 24

  IF ( KIND(T1%t%I)   .NE. 4 )                 ERROR STOP 25
  IF ( SIZE(T1%t%I)   .NE. 4 )                 ERROR STOP 26

  IF ( KIND(T1%t%R)   .NE. 4 )                 ERROR STOP 28
  IF ( SIZE(T1%t%R)   .NE. 4 )                 ERROR STOP 29

  IF ( KIND(T1%t%Z)   .NE. 4 )                 ERROR STOP 31
  IF ( SIZE(T1%t%Z)   .NE. 4 )                 ERROR STOP 32

  IF ( LEN(T1%t%C)    .NE. 4 )                 ERROR STOP 34
  IF ( SIZE(T1%t%C)   .NE. 4 )                 ERROR STOP 35

  IF ( ASSOCIATED(T1%t%ProcPtr) )              ERROR STOP 37
  if (any(T1%t%I /= -4))    error stop 27
  if (any(T1%t%r /= -4))    error stop 30
  if (any(T1%t%z /= (-4,4)))    error stop 33
  if (t1%t%c(1) /= '????') error stop 36
  if (any (t1%t%ll)) error stop 38



  IF ( T2%T%K      .NE. 8 )                 ERROR STOP 41
  IF ( T2%T%L      .NE. 8 )                 ERROR STOP 42
  IF ( T2%K2        .NE. 8 )                 ERROR STOP 43
  IF ( T2%L2        .NE. 8 )                 ERROR STOP 44

  IF ( KIND(T2%t%I)   .NE. 8 )                 ERROR STOP 45
  IF ( SIZE(T2%t%I)   .NE. 8 )                 ERROR STOP 46
  IF ( ANY(T2%t%I     .NE. -8))                ERROR STOP 47

  IF ( KIND(T2%t%R)   .NE. 8 )                 ERROR STOP 48
  IF ( SIZE(T2%t%R)   .NE. 8 )                 ERROR STOP 49
  IF ( ANY(T2%t%R     .NE. -8))                ERROR STOP 40

  IF ( KIND(T2%t%Z)   .NE. 8 )                 ERROR STOP 51
  IF ( SIZE(T2%t%Z)   .NE. 8 )                 ERROR STOP 52
  IF ( ANY(T2%t%Z     .NE. (-8,8)))            ERROR STOP 53

  IF ( LEN(T2%t%C)    .NE. 8 )                 ERROR STOP 54
  IF ( SIZE(T2%t%C)   .NE. 8 )                 ERROR STOP 55
  IF ( ANY(T2%t%C     .NE. "????????" ))       ERROR STOP 56

  IF ( ASSOCIATED(T2%t%ProcPtr) )              ERROR STOP 57



  END

