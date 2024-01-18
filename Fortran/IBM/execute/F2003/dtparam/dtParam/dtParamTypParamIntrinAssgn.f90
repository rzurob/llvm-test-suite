!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 27, 2006
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
!*  If necessary, the value is converted according to the rules of intrinsic
!*  assignment (7.4.1.3) to a value of the same kind as the type parameter.
!*  (failed at KIND(K1))
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dtParamTypParamIntrinAssgn

  TYPE :: DT0(K1, K2, K, K4, K8)
    INTEGER(KIND=1_2), KIND :: K1=1_2
    INTEGER(KIND=2_1), KIND :: K2=2_1
    INTEGER,           KIND :: K =4_1
    INTEGER(KIND=4_8), KIND :: K4=4_8
    INTEGER(KIND=8_1), KIND :: K8=8_1

    INTEGER(KIND=KIND(K1)) :: IK1=K1
    INTEGER(KIND=KIND(K2)) :: IK2=K2
    INTEGER(KIND=KIND(K))  :: IK=K
    INTEGER(KIND=KIND(K4)) :: IK4=K4
    INTEGER(KIND=KIND(K8)) :: IK8=K8
  END TYPE


  TYPE(DT0) :: T0

  IF ( KIND(T0%IK1) .NE. 1 )      STOP 11
  IF ( T0%IK1       .NE. 1 )      STOP 12
  IF ( KIND(T0%IK2) .NE. 2 )      STOP 13
  IF ( T0%IK2       .NE. 2 )      STOP 14
  IF ( KIND(T0%IK)  .NE. 4 )      STOP 15
  IF ( T0%IK        .NE. 4 )      STOP 16
  IF ( KIND(T0%IK4) .NE. 4 )      STOP 17
  IF ( T0%IK4       .NE. 4 )      STOP 18
  IF ( KIND(T0%IK8) .NE. 8 )      STOP 19
  IF ( T0%IK8       .NE. 8 )      STOP 10


  END


