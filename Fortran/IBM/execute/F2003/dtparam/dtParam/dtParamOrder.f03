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
!*  The type parameter order of a nonextended type is the order of the type parameter list
!*  in the derived type definition
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamOrder
  IMPLICIT NONE

  TYPE DT1(K1, K2, K3)
    INTEGER,          KIND :: K1
    INTEGER(KIND=K1), KIND :: K2
    INTEGER(KIND=K2), KIND :: K3
  END TYPE

  TYPE(DT1(2,4,8)) :: T1

  TYPE DT2(L1, L2, L3)
    INTEGER(KIND=8), LEN :: L1
    INTEGER(KIND=2), LEN :: L2
    INTEGER(KIND=4), LEN :: L3
    INTEGER(KIND=1)      :: Arr(L1:L2, L2:L3)
  END TYPE

  TYPE(DT2(2,4,8)), TARGET  :: T2
  TYPE(DT2(:,4,:)), POINTER :: T3

  IF (KIND(T1%K1) .NE. 4 )                 ERROR STOP 11
  IF (     T1%K1  .NE. 2 )                 ERROR STOP 12
  IF (KIND(T1%K2) .NE. 2 )                 ERROR STOP 13
  IF (     T1%K2  .NE. 4 )                 ERROR STOP 14
  IF (KIND(T1%K3) .NE. 4 )                 ERROR STOP 15
  IF (     T1%K3  .NE. 8 )                 ERROR STOP 16


  IF (KIND(T2%L1) .NE. 8 )                 ERROR STOP 21
  IF (     T2%L1  .NE. 2 )                 ERROR STOP 22
  IF (KIND(T2%L2) .NE. 2 )                 ERROR STOP 23
  IF (     T2%L2  .NE. 4 )                 ERROR STOP 24
  IF (KIND(T2%L3) .NE. 4 )                 ERROR STOP 25
  IF (     T2%L3  .NE. 8 )                 ERROR STOP 26
  IF (ANY(LBOUND(T2%Arr) .NE. (/2,4/) ))   ERROR STOP 26
  IF (ANY(UBOUND(T2%Arr) .NE. (/4,8/) ))   ERROR STOP 26

  T3 => T2
  IF (KIND(T3%L1) .NE. 8 )                 ERROR STOP 31
  IF (     T3%L1  .NE. 2 )                 ERROR STOP 32
  IF (KIND(T3%L2) .NE. 2 )                 ERROR STOP 33
  IF (     T3%L2  .NE. 4 )                 ERROR STOP 34
  IF (KIND(T3%L3) .NE. 4 )                 ERROR STOP 35
  IF (     T3%L3  .NE. 8 )                 ERROR STOP 36
  IF (ANY(LBOUND(T3%Arr) .NE. (/2,4/) ))   ERROR STOP 36
  IF (ANY(UBOUND(T3%Arr) .NE. (/4,8/) ))   ERROR STOP 36



  END

