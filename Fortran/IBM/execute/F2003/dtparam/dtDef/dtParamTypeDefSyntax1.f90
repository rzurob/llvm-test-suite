!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDefSyntax1
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Nov. 23, 2005
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Syntax
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
!*  No data components 
!*
!*  (338858)
!
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDefSyntax1 
  IMPLICIT REAL(T)
  PARAMETER (Kind1=4)

  TYPE :: DT1(TP1, TP2, TP3)
    INTEGER(Kind1), KIND :: TP3
    INTEGER(Kind1), KIND :: TP1
    INTEGER(Kind1), KIND :: TP2
  END TYPE

  TYPE :: DT2(TP1, TP2, TP3)
    INTEGER(Kind1), LEN :: TP3
    INTEGER(Kind1), LEN :: TP2
    INTEGER(Kind1), LEN :: TP1
  END TYPE

  TYPE, EXTENDS(DT1) :: DT3(KP1, KP2, KP3)
    INTEGER(Kind1), LEN :: KP3 = Kind1
    INTEGER(Kind1), LEN :: KP2 = Kind1
    INTEGER(Kind1), LEN :: KP1 = Kind1
  END TYPE

  TYPE, EXTENDS(DT2) :: DT4
  END TYPE

  TYPE, EXTENDS(DT4) :: DT5(KP1, KP2, KP3)
    INTEGER(Kind1), KIND :: KP3 = Kind1
    INTEGER(Kind1), KIND :: KP2 = Kind1
    INTEGER(Kind1), KIND :: KP1 = Kind1
  END TYPE

  
  TYPE (DT1(0, 1, 2147483647))                                  :: T1 
  TYPE (DT2(0_8, -1_1, -2147483648))                            :: T2
  TYPE (DT3(0, 1, 2147483647, 0_8, -1_1, -2147483648 ))         :: T3
  TYPE (DT4(-1_1+1, 2_8-1_2, -2147483648))                      :: T4 
  TYPE (DT5(-1_1+1, 2_8-1_2, -2147483649_8+1, 1_8, 1_4, 1_2)):: T5 

  IF (T1%TP1 .NE. 0 )           STOP 11
  IF (T1%TP2 .NE. 1 )           STOP 12
  IF (T1%TP3 .NE. 2147483647 )  STOP 13

  IF (T2%TP1 .NE. 0 )           STOP 21
  IF (T2%TP2 .NE. -1 )          STOP 22
  IF (T2%TP3 .NE. -2147483648 ) STOP 23

  IF (T3%TP1 .NE. 0 )           STOP 31
  IF (T3%TP2 .NE. 1 )           STOP 32
  IF (T3%TP3 .NE. 2147483647 )  STOP 33
  IF (T3%KP1 .NE. 0 )           STOP 34
  IF (T3%KP2 .NE. -1 )          STOP 35
  IF (T3%KP3 .NE. -2147483648 ) STOP 36

  IF (T4%TP1 .NE. 0 )           STOP 41
  IF (T4%TP2 .NE. 1 )           STOP 42
  IF (T4%TP3 .NE. -2147483648 ) STOP 43

  IF (T5%TP1 .NE. 0 )           STOP 51
  IF (T5%TP2 .NE. 1 )           STOP 52
  IF (T5%TP3 .NE. -2147483648 ) STOP 53
  IF (T5%KP1 .NE. 1 )          STOP 54
  IF (T5%KP2 .NE. 1 )          STOP 55
  IF (T5%KP3 .NE. 1 )          STOP 56

  END

