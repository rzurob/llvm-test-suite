!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 12, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Sequence Type
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
!*  Parameterized sequence type
!*
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDefSeq1

  TYPE :: Seq0(K, L)
    INTEGER, KIND :: K=4
    INTEGER, Len  :: L=4
    SEQUENCE
  END TYPE

  TYPE :: Seq1(K, L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4

    SEQUENCE
    INTEGER(K)  :: I=K
    REAL(K)     :: R=K
    COMPLEX(K)  :: Cplx=(-K,K)
    LOGICAL(K)  :: LL=.TRUE.!_K
    CHARACTER(K):: C="A"
  END TYPE

  TYPE :: Seq2(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L

    SEQUENCE
    TYPE(Seq0(K, K))   :: Comp1=Seq0(k,k)()
    TYPE(Seq1(K, K))   :: Comp2=Seq1(k,k)(-1, -1.0, (1., -1.), .FALSE., "B")
  END TYPE

  TYPE :: Seq3(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L

    SEQUENCE
    TYPE(Seq0(K, K))   :: Comp1(K, L)=Seq0(k,k)()
    TYPE(Seq1(K, K))   :: Comp2(k, l)=Seq1(k,k)(-1, -1.0, (1., -1.), .FALSE., "B")
  END TYPE

  TYPE(Seq0(4, 4)) :: T0
  TYPE(Seq1(4, 4)) :: T1
  TYPE(Seq2(4, 4)) :: T2
  TYPE(Seq3(4, 4)) :: T3

  IF ( T0%K .NE. 4 ) ERROR STOP 11
  IF ( T0%L .NE. 4 ) ERROR STOP 11

  IF ( T1%K    .NE. 4 )         ERROR STOP 21
  IF ( T1%L    .NE. 4 )         ERROR STOP 22
  IF ( T1%I    .NE. 4 )         ERROR STOP 23
  IF ( T1%R    .NE. 4. )        ERROR STOP 24
  IF ( T1%Cplx .NE. (-4., 4.) ) ERROR STOP 25
  IF ( T1%LL   .NEQV. .TRUE. )  ERROR STOP 26
  IF ( T1%C    .NE. "A"  )      ERROR STOP 27

  IF ( T2%Comp1%K    .NE. 4 )         ERROR STOP 31
  IF ( T2%Comp1%L    .NE. 4 )         ERROR STOP 32
  IF ( T2%Comp2%K    .NE. 4 )         ERROR STOP 33
  IF ( T2%Comp2%L    .NE. 4 )         ERROR STOP 34
  IF ( T2%Comp2%I    .NE. -1 )        ERROR STOP 35
  IF ( T2%Comp2%R    .NE. -1. )       ERROR STOP 36
  IF ( T2%Comp2%Cplx .NE. (1., -1.) ) ERROR STOP 37
  IF ( T2%Comp2%LL   .NEQV. .FALSE. ) ERROR STOP 38
  IF ( T2%Comp2%C    .NE. "B"  )      ERROR STOP 38

  IF ( T3%Comp1%K    .NE. 4 )         ERROR STOP 41
  IF ( T3%Comp1%L    .NE. 4 )         ERROR STOP 42
  IF ( T3%Comp2%K    .NE. 4 )         ERROR STOP 43
  IF ( T3%Comp2%L    .NE. 4 )         ERROR STOP 44
  IF ( ANY(T3%Comp2%I    .NE. -1 ))        ERROR STOP 45
  IF ( ANY(T3%Comp2%R    .NE. -1. ))       ERROR STOP 46
  IF ( ANY(T3%Comp2%Cplx .NE. (1., -1.)) ) ERROR STOP 47
  IF ( ANY(T3%Comp2%LL   .NEQV. .FALSE. )) ERROR STOP 48
  IF ( ANY(T3%Comp2%C    .NE. "B"  ))      ERROR STOP 49
  IF ( ANY(SHAPE(T3%Comp1)  .NE. (/T3%Comp1%K, T3%Comp1%L/)) ) ERROR STOP 51
  IF ( ANY(SHAPE(T3%Comp2)  .NE. (/T3%Comp2%K, T3%Comp2%L/)) ) ERROR STOP 52


  END

