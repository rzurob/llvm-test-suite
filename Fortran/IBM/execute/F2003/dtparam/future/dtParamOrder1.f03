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
!*  (340293)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamOrder1
  IMPLICIT NONE

  TYPE DT1(K1, L1, K2, L2, K3, L3)
    INTEGER,          KIND :: K1
    INTEGER(KIND=K1), KIND :: K2   ! K1 has not been seen yet?
    INTEGER(KIND=K1), LEN  :: L1
    INTEGER(KIND=K2), KIND :: K3
    INTEGER(KIND=K2), LEN  :: L2
    INTEGER(KIND=K3), LEN  :: L3
!   INTEGER,          KIND :: K1
    INTEGER(KIND=1)        :: Arr(L1:L2, L2:L3)
  END TYPE

  TYPE(DT1( 2,  1,  4,  8,  8,  3)), TARGET  :: T1
  !         K1  L1  K2  L2  K3  L3


  IF (KIND(T1%K1) .NE. 4 )                 ERROR STOP 11
  IF (     T1%K1  .NE. 2 )                 ERROR STOP 12
  IF (KIND(T1%K2) .NE. 2 )                 ERROR STOP 13
  IF (     T1%K2  .NE. 4 )                 ERROR STOP 14
  IF (KIND(T1%K3) .NE. 4 )                 ERROR STOP 15
  IF (     T1%K3  .NE. 8 )                 ERROR STOP 16


  IF (KIND(T1%L1) .NE. 2 )                 ERROR STOP 21
  IF (     T1%L1  .NE. 1 )                 ERROR STOP 22
  IF (KIND(T1%L2) .NE. 4 )                 ERROR STOP 23
  IF (     T1%L2  .NE. 8 )                 ERROR STOP 24
  IF (KIND(T1%L3) .NE. 8 )                 ERROR STOP 25
  IF (     T1%L3  .NE. 3 )                 ERROR STOP 26

  IF (ANY(LBOUND(T1%Arr) .NE. (/1,1/) ))   ERROR STOP 27
  IF (ANY(UBOUND(T1%Arr) .NE. (/8,0/) ))   ERROR STOP 28
! IF (ANY(LBOUND(T1%Arr) .NE. (/1,8/) ))   ERROR STOP 27
! IF (ANY(UBOUND(T1%Arr) .NE. (/8,4/) ))   ERROR STOP 28

  END

