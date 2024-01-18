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
!*  Parameterized sequence type - pointer/allocatable
!*
!*  (Syntax err/ice/339653)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDefSeq2

  TYPE :: Seq0(K, L)
    INTEGER, KIND :: K
    INTEGER, Len  :: L
    SEQUENCE
  END TYPE

  TYPE :: Seq1(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L

    SEQUENCE
    INTEGER(K),    ALLOCATABLE  :: I
    REAL(K),       POINTER      :: R => NULL()
    COMPLEX(K),    ALLOCATABLE  :: Cplx
    LOGICAL(K),    ALLOCATABLE  :: LL
    CHARACTER(K),  ALLOCATABLE  :: C
  END TYPE

  TYPE :: Seq2(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L

    SEQUENCE
    TYPE(Seq0(K, K)), ALLOCATABLE :: Comp1
    TYPE(Seq1(K, K)), POINTER     :: Comp2
  END TYPE

  TYPE :: Seq3(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L

    SEQUENCE
    TYPE(Seq0(K, K)), ALLOCATABLE :: Comp1(:)
    TYPE(Seq1(K, K)), POINTER     :: Comp2(:)
    TYPE(Seq1(K, :)), POINTER     :: Comp3(:)
  END TYPE

  TYPE(Seq0(4, 4)), TARGET      :: T0
  TYPE(Seq1(4, 4)), TARGET      :: T1(1)
  TYPE(Seq2(4, 4)), POINTER     :: T2
  TYPE(Seq2(4, 4)), TARGET      :: T3(2,2) =Seq2(4, 4)(NULL(), NULL())
  TYPE(Seq3(4, 4)), ALLOCATABLE :: T4

  T1 = Seq1(4, 4)(-1, NULL(), (1., -1.), .FALSE., "B")

  IF ( T1(1)%K    .NE. 4 )         ERROR STOP 21
  IF ( T1(1)%L    .NE. 4 )         ERROR STOP 22
  IF ( T1(1)%I    .NE. -1 )        ERROR STOP 23
  IF ( ASSOCIATED(T1(1)%R))        ERROR STOP 24
  IF ( T1(1)%Cplx .NE. (1., -1.) ) ERROR STOP 25
  IF ( T1(1)%LL   .NEQV. .FALSE. ) ERROR STOP 26
  IF ( T1(1)%C    .NE. "B"  )      ERROR STOP 27

  ALLOCATE(T2, SOURCE=Seq2(4, 4)(T0, T1(1)))

  IF ( T2%Comp1%K    .NE. 4 )         ERROR STOP 31
  IF ( T2%Comp1%L    .NE. 4 )         ERROR STOP 32
  IF ( T2%Comp2%K    .NE. 4 )         ERROR STOP 33
  IF ( T2%Comp2%L    .NE. 4 )         ERROR STOP 34
  IF ( T2%Comp2%K    .NE. 4 )         ERROR STOP 35
  IF ( T2%Comp2%L    .NE. 4 )         ERROR STOP 36
  IF ( T2%Comp2%I    .NE. -1 )        ERROR STOP 37
  IF ( ASSOCIATED(T2%Comp2%R))        ERROR STOP 38
  IF ( T2%Comp2%Cplx .NE. (1., -1.) ) ERROR STOP 39
  IF ( T2%Comp2%LL   .NEQV. .FALSE. ) ERROR STOP 40
  IF ( T2%Comp2%C    .NE. "B"  )      ERROR STOP 41

  ALLOCATE(T4, SOURCE=Seq3(4, 4)([T0], T1, T1))

  IF ( T4%Comp1(1)%K    .NE. 4  )         ERROR STOP 51
  IF ( T4%Comp1(1)%L    .NE. 4  )         ERROR STOP 52
  IF ( T4%Comp2(1)%K    .NE. 4  )         ERROR STOP 53
  IF ( T4%Comp2(1)%L    .NE. 4  )         ERROR STOP 54
  IF ( T4%Comp2(1)%K    .NE. 4  )         ERROR STOP 55
  IF ( T4%Comp2(1)%L    .NE. 4  )         ERROR STOP 56
  IF ( T4%Comp2(1)%I    .NE. -1 )         ERROR STOP 57
  IF ( ASSOCIATED(T4%Comp2(1)%R))         ERROR STOP 58
  IF ( T4%Comp2(1)%Cplx .NE. (1., -1.))   ERROR STOP 59
  IF ( T4%Comp2(1)%LL   .NEQV. .FALSE. )  ERROR STOP 60
  IF ( T4%Comp2(1)%C    .NE. "B"   )      ERROR STOP 61
  IF ( T4%Comp3(1)%K    .NE. 4  )         ERROR STOP 62
  IF ( T4%Comp3(1)%L    .NE. 4  )         ERROR STOP 63
  IF ( T4%Comp3(1)%K    .NE. 4  )         ERROR STOP 64
  IF ( T4%Comp3(1)%L    .NE. 4  )         ERROR STOP 65
  IF ( T4%Comp3(1)%I    .NE. -1 )         ERROR STOP 66
  IF ( ASSOCIATED(T4%Comp3(1)%R) )        ERROR STOP 67
  IF ( T4%Comp3(1)%Cplx .NE. (1., -1.) )  ERROR STOP 68
  IF ( T4%Comp3(1)%LL   .NEQV. .FALSE. )  ERROR STOP 70
  IF ( T4%Comp3(1)%C    .NE. "B"  )       ERROR STOP 71

  IF ( ANY(SHAPE(T4%Comp1)  .NE. [1] ) ) ERROR STOP 81
  IF ( ANY(SHAPE(T4%Comp2)  .NE. 1 ) ) ERROR STOP 82
  IF ( ANY(SHAPE(T4%Comp3)  .NE. 1 ) ) ERROR STOP 82


  END

