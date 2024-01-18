!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 29, 2005
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
!*  syntax of derived type stmt
!*
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
  PARAMETER (KC=4)

  TYPE DT1(K1)
    INTEGER(KC), KIND :: K1=KC
  END TYPE DT1

  TYPE, EXTENDS(DT1) :: DT2(K, L)
    INTEGER, LEN  :: L
    INTEGER, KIND :: K
    PRIVATE
    INTEGER(K+K) :: Arr(K:L)
  END TYPE DT2

  END MODULE

  MODULE M1
  USE M

  TYPE, PUBLIC, EXTENDS(DT2)  :: DT3(K2)
    INTEGER, KIND :: K2
    CHARACTER(K1+K+K2) :: C(L)="1"
  END TYPE DT3


  END MODULE

  PROGRAM dtParamTypeDefSyntax6
  USE M1
  TYPE(DT3(K2=2,K=2,L=2,K1=2)) :: T

  IF (LEN(T%C)      .NE. 6  )  STOP 11
  IF (SIZE(T%C)     .NE. 2  )  STOP 12
  IF (TRIM(T%C(1)) .NE. "1" )  STOP 13
  IF (TRIM(T%C(2)) .NE. "1" )  STOP 14

  END

