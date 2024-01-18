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
!*  Parameterized sequence type - caracter/Assumed type parameters
!*
!*  (Syntax err/ice  --  339774)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDefSeq4

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
  TYPE(Seq3(4, 4)), ALLOCATABLE :: T3
  TYPE(Seq4(4, 4)), POINTER     :: T4(:)

! TYPE(Seq1(4, 4)), TARGET      :: Arg1
! TYPE(Seq2(4, 4)), POINTER     :: Arg2
! TYPE(Seq3(4, 4)), TARGET      :: Arg3
! TYPE(Seq4(4, 4)), POINTER     :: Arg4(:)

  T1 = Seq1(4, 4)("1234", CTar)
  ALLOCATE(T2, SOURCE=Seq2(4, 4)("1234", CTar))
  ALLOCATE(T3, SOURCE=Seq3(4, 4)((/"1234", "4321"/), CArr))
  ALLOCATE(T4(4), SOURCE=Seq4(4, 4)((/"1234", "4321"/), CArr))

  CALL Sub(T1, T2, T3, T4)

  CONTAINS

  SUBROUTINE Sub(Arg1, Arg2, Arg3, Arg4)
  TYPE(Seq1(4, *)), TARGET      :: Arg1
  TYPE(Seq2(4, :)), POINTER     :: Arg2
  TYPE(Seq3(4, *)), TARGET      :: Arg3
  TYPE(Seq4(4, *)), POINTER     :: Arg4(:)

  IF ( LEN(Arg1%C1)   .NE. 4 )    ERROR STOP 11
  IF ( LEN(Arg1%C2)   .NE. 4 )    ERROR STOP 12
  IF ( Arg1%C1   .NE. "1234" )    ERROR STOP 13
  IF ( Arg1%C2   .NE. "abcd" )    ERROR STOP 14

  IF ( LEN(Arg2%C1)   .NE. 4 )    ERROR STOP 21
  IF ( LEN(Arg2%C2)   .NE. 4 )    ERROR STOP 22
  IF ( Arg1%C1   .NE. "1234" )    ERROR STOP 23
  IF ( Arg1%C2   .NE. "abcd" )    ERROR STOP 24

  IF ( LEN(Arg3%C1)  .NE. 4 )           ERROR STOP 31
  IF ( LEN(Arg3%C2)  .NE. 4 )           ERROR STOP 32
  IF ( ANY(Arg3%C1   .NE. ["1234", '4321'] ))     ERROR STOP 33
  IF ( ANY(Arg3%C2   .NE. "abcd" ))     ERROR STOP 34
  IF ( ANY(SHAPE(Arg3%C1) .NE. (/2/)) ) ERROR STOP 35
  IF ( ANY(SHAPE(Arg3%C2) .NE. (/4/)) ) ERROR STOP 36

  IF ( LEN(Arg4(1)%C1)  .NE. 4 )           ERROR STOP 41
  IF ( LEN(Arg4(2)%C2)  .NE. 4 )           ERROR STOP 42
  IF ( ANY(Arg4(3)%C1   .NE. ["1234", '4321'] ))     ERROR STOP 43
  IF ( ANY(Arg4(4)%C2   .NE. "abcd" ))     ERROR STOP 44
  IF ( ANY(SHAPE(Arg4(1)%C1) .NE. (/2/)) ) ERROR STOP 45
  IF ( ANY(SHAPE(Arg4(1)%C2) .NE. (/4/)) ) ERROR STOP 46
  IF ( ANY(SHAPE(Arg4)       .NE. (/4/)) ) ERROR STOP 47

  END SUBROUTINE

  END

