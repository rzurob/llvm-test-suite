!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 23, 2006
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
!*  The type parameter order of an extended type consists of the type parameter order of
!*  its parent type followed by any additional type parameters in the order of the type
!*  parameter list in the derived-type definition.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamOrder3
  IMPLICIT NONE


  TYPE DT1(K1, L1)
    INTEGER,          KIND :: K1
    INTEGER(KIND=K1), LEN  :: L1=K1
  ! INTEGER,          KIND :: K1
  END TYPE

  TYPE, EXTENDS(DT1) ::  DT2(K2, L2, L3)
    INTEGER,          KIND :: K2=K1
    INTEGER(KIND=K2), LEN  :: L2=K2
    INTEGER(KIND=K1), LEN  :: L3=K1
  ! INTEGER,          KIND :: K2=K1
    INTEGER(KIND=K1)       :: Arr(L1:L2, L2:L3)
  END TYPE

  TYPE(DT2(1))               :: T1
  TYPE(DT2(2,-1,1))          :: T2
  TYPE(DT2(2,-1,1,0))        :: T3
  TYPE(DT2(2,-1,1,L3=8,L2=2)) :: T4


  IF (KIND(T1%K1) .NE. 4 )                 ERROR STOP 10
  IF (     T1%K1  .NE. 1 )                 ERROR STOP 11
  IF (KIND(T1%K2) .NE. 4 )                 ERROR STOP 12
  IF (     T1%K2  .NE. 1 )                 ERROR STOP 13
  IF (KIND(T1%L1) .NE. 1 )                 ERROR STOP 14
  IF (     T1%L1  .NE. 1 )                 ERROR STOP 15
  IF (KIND(T1%L2) .NE. 1 )                 ERROR STOP 16
  IF (     T1%L2  .NE. 1 )                 ERROR STOP 17
  IF (KIND(T1%L3) .NE. 1 )                 ERROR STOP 18
  IF (     T1%L3  .NE. 1 )                 ERROR STOP 19
  IF (KIND(T1%Arr).NE. 1 )                 ERROR STOP 20
  IF (ANY(LBOUND(T1%Arr) .NE. (/1,1/) ))   ERROR STOP 21
  IF (ANY(UBOUND(T1%Arr) .NE. (/1,1/) ))   ERROR STOP 22

  IF (KIND(T2%K1) .NE. 4 )                 ERROR STOP 30
  IF (     T2%K1  .NE. 2 )                 ERROR STOP 31
  IF (KIND(T2%K2) .NE. 4 )                 ERROR STOP 32
  IF (     T2%K2  .NE. 1 )                 ERROR STOP 33
  IF (KIND(T2%L1) .NE. 2 )                 ERROR STOP 34
  IF (     T2%L1  .NE.-1 )                 ERROR STOP 35
  IF (KIND(T2%L2) .NE. 1 )                 ERROR STOP 36
  IF (     T2%L2  .NE. 1 )                 ERROR STOP 37
  IF (KIND(T2%L3) .NE. 2 )                 ERROR STOP 38
  IF (     T2%L3  .NE. 2 )                 ERROR STOP 39
  IF (KIND(T2%Arr).NE. 2 )                 ERROR STOP 40
  IF (ANY(LBOUND(T2%Arr) .NE. (/-1,1/) ))  ERROR STOP 41
  IF (ANY(UBOUND(T2%Arr) .NE. (/ 1,2/) ))  ERROR STOP 42

  IF (KIND(T3%K1) .NE. 4 )                 ERROR STOP 50
  IF (     T3%K1  .NE. 2 )                 ERROR STOP 51
  IF (KIND(T3%K2) .NE. 4 )                 ERROR STOP 52
  IF (     T3%K2  .NE. 1 )                 ERROR STOP 53
  IF (KIND(T3%L1) .NE. 2 )                 ERROR STOP 54
  IF (     T3%L1  .NE.-1 )                 ERROR STOP 55
  IF (KIND(T3%L2) .NE. 1 )                 ERROR STOP 56
  IF (     T3%L2  .NE. 0 )                 ERROR STOP 57
  IF (KIND(T3%L3) .NE. 2 )                 ERROR STOP 58
  IF (     T3%L3  .NE. 2 )                 ERROR STOP 59
  IF (KIND(T3%Arr).NE. 2 )                 ERROR STOP 60
  IF (ANY(LBOUND(T3%Arr) .NE. (/-1,0/) ))  ERROR STOP 61
  IF (ANY(UBOUND(T3%Arr) .NE. (/ 0,2/) ))  ERROR STOP 62

  IF (KIND(T4%K1) .NE. 4 )                 ERROR STOP 70
  IF (     T4%K1  .NE. 2 )                 ERROR STOP 71
  IF (KIND(T4%K2) .NE. 4 )                 ERROR STOP 72
  IF (     T4%K2  .NE. 1 )                 ERROR STOP 73
  IF (KIND(T4%L1) .NE. 2 )                 ERROR STOP 74
  IF (     T4%L1  .NE.-1 )                 ERROR STOP 75
  IF (KIND(T4%L2) .NE. 1 )                 ERROR STOP 76
  IF (     T4%L2  .NE. 2 )                 ERROR STOP 77
  IF (KIND(T4%L3) .NE. 2 )                 ERROR STOP 78
  IF (     T4%L3  .NE. 8 )                 ERROR STOP 79
  IF (KIND(T4%Arr).NE. 2 )                 ERROR STOP 80
  IF (ANY(LBOUND(T4%Arr) .NE. (/-1,2/) ))  ERROR STOP 81
  IF (ANY(UBOUND(T4%Arr) .NE. (/ 2,8/) ))  ERROR STOP 82




  END


