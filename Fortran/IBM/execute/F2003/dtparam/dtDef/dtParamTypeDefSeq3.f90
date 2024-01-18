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
!*  Parameterized sequence type - caracter
!*
!*  (Syntax err/ice/340185)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDefSeq3

  TYPE :: Seq1(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L

    SEQUENCE
    CHARACTER(K),  ALLOCATABLE  :: C1
    CHARACTER(K),  POINTER      :: C2
  END TYPE

  TYPE :: Seq2(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L

    SEQUENCE
    CHARACTER(L),  ALLOCATABLE  :: C1
    CHARACTER(L),  POINTER      :: C2
  END TYPE

  TYPE :: Seq3(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L

    SEQUENCE
    CHARACTER(L),  ALLOCATABLE  :: C1(:)
    CHARACTER(L),  POINTER      :: C2(:)
  END TYPE

  TYPE :: Seq4(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L

    SEQUENCE
    CHARACTER(:),  ALLOCATABLE  :: C1(:)
    CHARACTER(:),  POINTER      :: C2(:)
  END TYPE

  CHARACTER(4),     TARGET      :: CTar = "abcd", CArr(4)="abcd"
  TYPE(Seq1(4, 4)), TARGET      :: T1
  TYPE(Seq2(4, :)), POINTER     :: T2
  TYPE(Seq3(4, 4)), ALLOCATABLE, TARGET      :: T3
  TYPE(Seq4(4, 4)), POINTER     :: T4(:)

  T1 = Seq1(4, 4)("1234", CTar)
  IF ( LEN(T1%C1)   .NE. 4 )    STOP 11
  IF ( LEN(T1%C2)   .NE. 4 )    STOP 12
  IF ( T1%C1   .NE. "1234" )    STOP 13
  IF ( T1%C2   .NE. "abcd" )    STOP 14

  ALLOCATE(T2, SOURCE=Seq2(4, 4)("1234", CTar))
  IF ( LEN(T2%C1)   .NE. 4 )    STOP 21
  IF ( LEN(T2%C2)   .NE. 4 )    STOP 22
  IF ( T1%C1   .NE. "1234" )    STOP 23
  IF ( T1%C2   .NE. "abcd" )    STOP 24

  ALLOCATE(T3, SOURCE=Seq3(4, 4)((/"1234", "4321"/), CArr))
  IF ( LEN(T3%C1)  .NE. 4 )           STOP 31
  IF ( LEN(T3%C2)  .NE. 4 )           STOP 32
  IF ( ANY(T3%C1   .NE. (/"1234",'4321'/) ))     STOP 33
  IF ( ANY(T3%C2   .NE. "abcd" ))     STOP 34
  IF ( ANY(SHAPE(T3%C1) .NE. (/2/)) ) STOP 35
  IF ( ANY(SHAPE(T3%C2) .NE. (/4/)) ) STOP 36

  ALLOCATE(T4(4), SOURCE=Seq4(4, 4)((/"1234", "4321"/), CArr))
  IF ( LEN(T4(1)%C1)  .NE. 4 )           STOP 41
  IF ( LEN(T4(2)%C2)  .NE. 4 )           STOP 42
  IF ( ANY(T4(3)%C1   .NE. ["1234", '4321'] ))     STOP 43
  IF ( ANY(T4(4)%C2   .NE. "abcd" ))     STOP 44
  IF ( ANY(SHAPE(T4(2)%C1) .NE. (/2/)) ) STOP 45
  IF ( ANY(SHAPE(T4(3)%C2) .NE. (/4/)) ) STOP 46
  IF ( ANY(SHAPE(T4)  .NE. (/4/)) )      STOP 47

  END

