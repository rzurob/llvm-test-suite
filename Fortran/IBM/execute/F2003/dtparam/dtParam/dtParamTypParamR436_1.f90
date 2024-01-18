!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 30, 2006
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
!*  R436 type-param-decl is type-param-name [ = scalar-int-initialization-expr ]
!*  - array constructor
!*  (failed)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypParamR436_1

  INTEGER, PARAMETER :: F=4

  TYPE DT1(K, L)
    INTEGER(KIND=F), KIND :: K=F
    INTEGER(KIND=F), LEN  :: L=F
    INTEGER(KIND=K)       :: Arr1(K)= (/(K, i=1,K)/)
    INTEGER(KIND=K)       :: Arr2(L)= -K
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2, L2)
    INTEGER(KIND=K), KIND :: K2=K
    INTEGER(KIND=K), LEN  :: L2=K
    INTEGER(KIND=K)       :: Arr3(K)=  K
    INTEGER(KIND=K)       :: Arr4(L)= -K
  END TYPE


  TYPE (DT1)    :: T1
  TYPE (DT2)    :: T2


  IF (KIND(T1%K) .NE. F ) STOP 11
  IF (T1%K       .NE. F ) STOP 12

  IF (KIND(T1%L) .NE. F ) STOP 13
  IF (T1%L       .NE. F ) STOP 14

  IF (KIND(T1%Arr1) .NE. T1%K )           STOP 15
  IF (ANY(T1%Arr1   .NE. T1%K) )          STOP 16
  IF (ANY(SHAPE(T1%Arr1) .NE. (/T1%K/)) ) STOP 17

  IF (KIND(T1%Arr2) .NE. T1%K )           STOP 15
  IF (ANY(T1%Arr2   .NE. -T1%K))          STOP 16
  IF (ANY(SHAPE(T1%Arr2) .NE. (/T1%L/)) ) STOP 17


  IF (KIND(T2%K) .NE. T2%K ) STOP 21
  IF (T2%K       .NE. T2%K ) STOP 22

  IF (KIND(T2%L) .NE. T2%K ) STOP 23
  IF (T2%L       .NE. T2%K ) STOP 24

  IF (KIND(T2%Arr3) .NE. T2%K )           STOP 25
  IF (ANY(T2%Arr3   .NE. T2%K) )          STOP 26
  IF (ANY(SHAPE(T2%Arr3) .NE. (/T2%K/)) ) STOP 27

  IF (KIND(T2%Arr4) .NE. T2%K )           STOP 25
  IF (ANY(T2%Arr4   .NE. -T2%K))          STOP 26
  IF (ANY(SHAPE(T2%Arr4) .NE. (/T1%L/)) ) STOP 27



  END


