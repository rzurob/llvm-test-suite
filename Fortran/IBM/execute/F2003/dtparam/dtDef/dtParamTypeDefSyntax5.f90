!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDefSyntax5
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Nov. 29, 2005
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
!*  syntax of derived type stmt 
!*
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  IMPLICIT REAL(K)

  TYPE, PUBLIC  :: DT1(K)
    INTEGER, KIND :: K
  END TYPE

  TYPE, PUBLIC  :: DT2(K)
    INTEGER, KIND :: K
    PRIVATE
  END TYPE

  TYPE, PUBLIC  :: DT3(K)
    INTEGER, KIND :: K
    PRIVATE
    SEQUENCE
  END TYPE

  TYPE, PRIVATE  :: DT4(K)
    INTEGER, KIND :: K
    SEQUENCE
    PRIVATE
  END TYPE

  TYPE, PUBLIC  :: DT5(K)
    INTEGER, KIND :: K
    SEQUENCE
    PRIVATE
    REAL (K) :: R=K
  END TYPE

  TYPE, PUBLIC, ABSTRACT  :: DT6(K)
    INTEGER, KIND :: K
    PRIVATE
    REAL (K) :: R = K
  END TYPE

  TYPE, PRIVATE, EXTENDS(DT6)  :: DT7(K1)
    INTEGER, KIND :: K1
    PRIVATE
    REAL (K1) :: T = K
    CONTAINS
    PROCEDURE, PASS :: S
  END TYPE

  TYPE(DT4(8))   :: T4
  TYPE(DT7(4,8)), SAVE :: T7

  CONTAINS

  SUBROUTINE S(Arg)
  CLASS(DT7(4,4)) :: Arg
  END SUBROUTINE

  SUBROUTINE Check()

  TYPE(DT1(1)) :: T1
  TYPE(DT2(2)) :: T2
  TYPE(DT3(4)) :: T3
  TYPE(DT5(8)) :: T5
 
  IF ( T1%K .NE. 1 ) STOP 11
  IF ( T2%K .NE. 2 ) STOP 12
  IF ( T3%K .NE. 4 ) STOP 13
  IF ( T4%K .NE. 8 ) STOP 14

  IF ( T5%K .NE. 8 ) STOP 21
  IF ( T5%R .NE. 8 ) STOP 22

  IF ( T7%K  .NE. 4 ) STOP 31
  IF ( T7%K1 .NE. 8 ) STOP 32
  IF ( T7%R  .NE. 4 ) STOP 33
  IF ( T7%T  .NE. 4 ) STOP 34

  END SUBROUTINE

  END MODULE
  
  PROGRAM dtParamTypeDefSyntax5 
  USE M

  CALL Check()
 
  END

