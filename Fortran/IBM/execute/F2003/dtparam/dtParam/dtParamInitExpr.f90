!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 22, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Derived type parameters
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
!*  Init expression for type parameters
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamInitExpr
  IMPLICIT NONE

  INTEGER :: I


  TYPE DT1(K1, K2, K3)
    INTEGER,          KIND :: K1
    INTEGER(KIND=K1), KIND :: K2=MAXVAL((/1, 2*K1/))
    INTEGER(KIND=K2), KIND :: K3=MAXVAL((/4*K1,2*K2/))
  END TYPE


  TYPE (DT1(K1=1))                :: T1
  TYPE (DT1(K1=1, K2=1 ))         :: T2
  TYPE (DT1(K1=1, K2=2, K3=1))    :: T3



  IF (KIND(T1%K1) .NE. 4 ) STOP 11
  IF (     T1%K1  .NE. 1 ) STOP 12
  IF (KIND(T1%K2) .NE. 1 ) STOP 13
  IF (     T1%K2  .NE. 2 ) STOP 14
  IF (KIND(T1%K3) .NE. 2 ) STOP 15
  IF (     T1%K3  .NE. 4 ) STOP 16

  IF (KIND(T2%K1) .NE. 4 ) STOP 21
  IF (     T2%K1  .NE. 1 ) STOP 22
  IF (KIND(T2%K2) .NE. 1 ) STOP 23
  IF (     T2%K2  .NE. 1 ) STOP 24
  IF (KIND(T2%K3) .NE. 1 ) STOP 25
  IF (     T2%K3  .NE. 4 ) STOP 26

  IF (KIND(T3%K1) .NE. 4 ) STOP 31
  IF (     T3%K1  .NE. 1 ) STOP 32
  IF (KIND(T3%K2) .NE. 1 ) STOP 33
  IF (     T3%K2  .NE. 2 ) STOP 34
  IF (KIND(T3%K3) .NE. 2 ) STOP 35
  IF (     T3%K3  .NE. 1 ) STOP 36



  END


