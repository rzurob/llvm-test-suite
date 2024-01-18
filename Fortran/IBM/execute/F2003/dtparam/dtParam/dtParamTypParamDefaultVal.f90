!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 15, 2005
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
!*  If a type-param-decl has a scalar-int-initialization-expr, the type
!*  parameter has a default value which is specified by the expression.
!*
!*  (init expr contains type paramters?)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dtParamTypParamDefaultVal

  PARAMETER (I1Max= 127)
  PARAMETER (I1Min=-128)

  PARAMETER (I2Max= 32767)
  PARAMETER (I2Min=-32768)

  PARAMETER (I4Max= 2147483647)
  PARAMETER (I4Min=-2147483648)

  INTEGER(8), PARAMETER ::I8Max= 9223372036854775807_8
  INTEGER(8), PARAMETER ::I8Min=-9223372036854775808_8

  TYPE :: DT0(K1, K2, K, K4, K8)
    !INTEGER(KIND=1+K1), KIND :: K1=I1Min+K1+2   ! illeagal
    INTEGER(KIND=1),    KIND :: K1
    INTEGER(KIND=K1+1), KIND :: K2=I2Min+I2Max+K1+2  !(k1+1)
    INTEGER,            KIND :: K =I4Min+I4MAX+K2+3  !(k2+1==k1+3)
    INTEGER(KIND=4),  KIND :: K4=I4Min+I4MAX+K+1   !(k1+3)
    INTEGER(KIND=8), KIND :: K8=I8Min+I8MAX+K+5   !(k1+7)

    INTEGER(KIND=1) :: IK1=K1
    INTEGER(KIND=2) :: IK2=K2
    INTEGER  :: IK=K
    INTEGER(KIND=4) :: IK4=K4
    INTEGER(KIND=8) :: IK8=K8
  END TYPE

  TYPE :: DT1(K1, K2, K, K4, K8, L1, L2, L, L4, L8)
    INTEGER(KIND=1), KIND :: K1=I1Max
    INTEGER(KIND=2), KIND :: K2=I2Max
    INTEGER,         KIND :: K =I4Max
    INTEGER(KIND=4), KIND :: K4=I4Max
    INTEGER(KIND=8), KIND :: K8=I8Max

    INTEGER(KIND=1), LEN  :: L1=K1
    INTEGER(KIND=2), LEN  :: L2=K2
    INTEGER,         LEN  :: L =K
    INTEGER(KIND=4), LEN  :: L4=K4
    INTEGER(KIND=8), LEN  :: L8=K8

    CHARACTER(LEN=L1) :: CL1="1"
    CHARACTER(LEN=L2) :: CL2="2"
  END TYPE


  TYPE(DT0(1)) :: T1
  TYPE(DT1) :: T2

  IF ( KIND(T1%IK1) .NE. 1 )      STOP 11
  IF ( T1%IK1       .NE. 1 )      STOP 12

  IF ( KIND(T1%IK2) .NE. 2 )      STOP 21
  IF ( T1%IK2       .NE. 2 )      STOP 22

  IF ( KIND(T1%IK)  .NE. 4 )      STOP 31
  IF ( T1%IK        .NE. 4 )      STOP 32

  IF ( KIND(T1%IK4) .NE. 4 )      STOP 41
  IF ( T1%IK4       .NE. 4 )      STOP 42

  IF ( KIND(T1%IK8) .NE. 8 )      STOP 51
  IF ( T1%IK8       .NE. 8 )      STOP 52


  IF ( T2%L1        .NE. I1Max )  STOP 61
  IF ( LEN(T2%CL1)  .NE. I1Max )  STOP 62
  IF ( TRIM(T2%CL1) .NE. "1" )    STOP 63

  IF ( T2%L2        .NE. I2Max )  STOP 71
  IF ( LEN(T2%CL2)  .NE. I2Max )  STOP 72
  IF ( TRIM(T2%CL2) .NE. "2" )    STOP 73

  IF ( T2%L         .NE. I4Max )  STOP 81
  IF ( T2%L4        .NE. I4Max )  STOP 91
  IF ( T2%L8        .NE. I8Max )  STOP 101


  END


