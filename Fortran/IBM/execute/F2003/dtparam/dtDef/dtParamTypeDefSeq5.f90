!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDefSeq5
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
!*  Parameterized sequence type - components of sequence type
!*
!*  (Syntax err/ice/339738)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDefSeq5

  TYPE :: Seq0(K, L)
    INTEGER, KIND :: K
    INTEGER, Len  :: L
    SEQUENCE
    CHARACTER(L)  :: Comp=""
  END TYPE

  TYPE :: Seq1(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    SEQUENCE
    TYPE(Seq0(K, :)), ALLOCATABLE :: Comp
  END TYPE

  TYPE :: Seq2(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L

    SEQUENCE
    TYPE(Seq1(K, :)), ALLOCATABLE :: Comp(:)
  END TYPE


  TYPE(Seq2(4, 4)), ALLOCATABLE :: T(:)

  ALLOCATE(T(1), SOURCE=(/Seq2(4, 4)((/Seq1(4, 4)(Seq0(4, 4)("1234"))/))/))

  IF ( .NOT. ALLOCATED(T) )       STOP 11
  IF ( ANY(SHAPE(T) .NE. (/1/)))  STOP 12
  IF ( T%K          .NE. 4 )      STOP 13
  IF ( T%L          .NE. 4 )      STOP 14

  IF ( ANY(SHAPE(T(1)%Comp) .NE. (/1/)))  STOP 21
  IF ( T(1)%Comp%K      .NE. 4 )     STOP 22
  IF ( T(1)%Comp%L      .NE. 4 )     STOP 23

  IF ( T(1)%Comp(1)%Comp%K      .NE. 4 )     STOP 32
  IF ( T(1)%Comp(1)%Comp%L      .NE. 4 )     STOP 33

  IF ( LEN(T(1)%Comp(1)%Comp%Comp)   .NE. 4 )       STOP 42
  IF ( T(1)%Comp(1)%Comp%Comp        .NE. "1234" )  STOP 43

  END

