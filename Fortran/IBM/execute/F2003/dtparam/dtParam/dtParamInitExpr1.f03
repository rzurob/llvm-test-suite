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
!*  (340286)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamInitExpr1
  IMPLICIT NONE

  TYPE DT1(K1, K2, K3)
    INTEGER,          KIND :: K1
    INTEGER(KIND=K1), KIND :: K2=MAXVAL((/1, 2*K1/))
    INTEGER(KIND=K2), KIND :: K3=MAXVAL((/4*K1,2*K2/))
  END TYPE

  INTEGER :: I

  TYPE, EXTENDS(DT1) :: DT2(L1, L2, L3)
    INTEGER(KIND=K1), LEN  :: L1=k1
    INTEGER(KIND=K2), LEN  :: L2=k2
    INTEGER(KIND=K3), LEN  :: L3=k3
    TYPE(DT1(K1, K2, K3))  :: Arr1(L1, L2, L3)=DT1(K1, K2, K3)()
  END TYPE

  TYPE (DT2(K1=1, K2=2, K3=1))                     :: V1
  TYPE (DT2(K1=1, K2=2, K3=1, L1=2))               :: V2
  TYPE (DT2(K1=1, K2=2, K3=1, L1=2, L2=1))         :: V3
  TYPE (DT2(K1=1, K2=2, K3=1, L1=2, L2=1, L3=3))   :: V4


  IF (KIND(V1%L1) .NE. 1 )                 ERROR STOP 11
  IF (     V1%L1  .NE. 1 )                 ERROR STOP 12
  IF (KIND(V1%L2) .NE. 2 )                 ERROR STOP 13
  IF (     V1%L2  .NE. 2 )                 ERROR STOP 14
  IF (KIND(V1%L3) .NE. 1 )                 ERROR STOP 15
  IF (     V1%L3  .NE. 1 )                 ERROR STOP 16
  IF (ANY( SHAPE(V1%Arr1) .NE. (/1,2,1/))) ERROR STOP 17

  IF (KIND(V2%L1) .NE. 1 )                 ERROR STOP 21
  IF (     V2%L1  .NE. 2 )                 ERROR STOP 22
  IF (KIND(V2%L2) .NE. 2 )                 ERROR STOP 23
  IF (     V2%L2  .NE. 2 )                 ERROR STOP 24
  IF (KIND(V2%L3) .NE. 1 )                 ERROR STOP 25
  IF (     V2%L3  .NE. 1 )                 ERROR STOP 26
  IF (ANY( SHAPE(V2%Arr1) .NE. (/2,2,1/))) ERROR STOP 27

  IF (KIND(V3%L1) .NE. 1 )                 ERROR STOP 31
  IF (     V3%L1  .NE. 2 )                 ERROR STOP 32
  IF (KIND(V3%L2) .NE. 2 )                 ERROR STOP 33
  IF (     V3%L2  .NE. 1 )                 ERROR STOP 34
  IF (KIND(V3%L3) .NE. 1 )                 ERROR STOP 35
  IF (     V3%L3  .NE. 1 )                 ERROR STOP 36
  IF (ANY( SHAPE(V3%Arr1) .NE. (/2,1,1/))) ERROR STOP 37

  IF (KIND(V4%L1) .NE. 1 )                 ERROR STOP 41
  IF (     V4%L1  .NE. 2 )                 ERROR STOP 42
  IF (KIND(V4%L2) .NE. 2 )                 ERROR STOP 43
  IF (     V4%L2  .NE. 1 )                 ERROR STOP 44
  IF (KIND(V4%L3) .NE. 1 )                 ERROR STOP 45
  IF (     V4%L3  .NE. 3 )                 ERROR STOP 46
  IF (ANY( SHAPE(V4%Arr1) .NE. (/2,1,3/))) ERROR STOP 47



  END

