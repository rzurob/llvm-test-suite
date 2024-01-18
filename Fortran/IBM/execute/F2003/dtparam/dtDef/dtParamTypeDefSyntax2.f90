!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDefSyntax2
!*
!*  DATE                       : Nov. 23, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Syntax
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
!*  Make use of type parameters from the parent component
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDefSyntax2
  IMPLICIT REAL(T)
  PARAMETER (Kind1=1)

  TYPE :: DT0
    INTEGER :: Kind=2
  END TYPE

  TYPE(DT0), PARAMETER :: Const=DT0()

  TYPE :: DT1(TP1, TP2, TP3)
    INTEGER(Kind1), KIND :: TP3 = 4
    INTEGER(Kind1), KIND :: TP1 = 4
    INTEGER(Kind1), KIND :: TP2 = 4
  END TYPE

  TYPE :: DT2(TP1, TP2, TP3)
    INTEGER(Kind1), LEN :: TP3 = 8
    INTEGER(Kind1), LEN :: TP2 = 8
    INTEGER(Kind1), LEN :: TP1 = 8
  END TYPE

  TYPE, EXTENDS(DT1) :: DT3(KP1, KP2, KP3)
    INTEGER(Kind1), LEN :: KP3 = Const%Kind
    INTEGER(Kind1), LEN :: KP2 = Const%Kind
    INTEGER(Kind1), LEN :: KP1 = Const%Kind
  END TYPE

  TYPE, EXTENDS(DT2) :: DT4
  END TYPE

  TYPE, EXTENDS(DT4) :: DT5(KP1)
    INTEGER(Const%Kind), KIND   :: KP1 = Const%Kind
    INTEGER(KP1)                :: Arr1(TP1+KP1)
    INTEGER, DIMENSION(TP2*KP1) :: Arr2
  END TYPE

  TYPE, EXTENDS(DT4) :: DT6(KP1)
    INTEGER, KIND :: KP1
    TYPE(DT4(KP1, KP1, KP1)) ::X1
    TYPE(DT4(TP1, TP1, TP1)) ::X2
  END TYPE


  TYPE (DT1(1, 1,  127))            :: T1
  TYPE (DT2)                         :: T2
  TYPE (DT3(KP1=1, KP2=1, KP3=1))    :: T3
  TYPE (DT4)                         :: T4
  TYPE (DT5)                         :: T5
  TYPE (DT6(KP1=5, TP1=7))           :: T6

  IF (T1%TP1 .NE. 1 )    STOP 11
  IF (T1%TP2 .NE.  1 )    STOP 12
  IF (T1%TP3 .NE.  127 )  STOP 13

  IF (T2%TP1 .NE.  8)     STOP 21
  IF (T2%TP2 .NE.  8)     STOP 22
  IF (T2%TP3 .NE.  8)     STOP 23

  IF (T3%KP1 .NE.  1)     STOP 31
  IF (T3%KP2 .NE.  1)     STOP 32
  IF (T3%KP3 .NE.  1)     STOP 33

  IF (T4%TP1 .NE.  8)     STOP 41
  IF (T4%TP2 .NE.  8)     STOP 42
  IF (T4%TP3 .NE.  8)     STOP 43

  IF (T5%TP1 .NE.  8)     STOP 51
  IF (T5%KP1 .NE.  2)     STOP 52
  IF (SIZE(T5%Arr1) .NE.  10)     STOP 53
  IF (SIZE(T5%Arr2) .NE.  16)     STOP 54

  IF (T6%X1%TP1 .NE.  5)     STOP 61
  IF (T6%X1%TP2 .NE.  5)     STOP 62
  IF (T6%X1%TP3 .NE.  5)     STOP 63
  IF (T6%X2%TP1 .NE.  7)     STOP 64
  IF (T6%X2%TP2 .NE.  7)     STOP 65
  IF (T6%X2%TP2 .NE.  7)     STOP 66


  END

