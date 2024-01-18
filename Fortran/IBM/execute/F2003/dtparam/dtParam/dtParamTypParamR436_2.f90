!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypParamR436_2
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
!*  -Structure constructor
!*  (failed)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypParamR436_2
  IMPLICIT NONE

  INTEGER :: I


  TYPE DT1(K, L)
    INTEGER,         KIND :: K
    INTEGER(KIND=K), LEN  :: L=k
    INTEGER(KIND=K)       :: Arr(L)= -K
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2, L2)
    INTEGER(KIND=K), KIND :: K2=k
    INTEGER(KIND=K), LEN  :: L2=k
    TYPE(DT1(K))          :: Arr1(L2)=DT1(K) ()
  END TYPE

  INTEGER, PARAMETER :: K=1

  TYPE (DT1(k=1))    :: T1
  TYPE (DT2(K=1))    :: T2

  IF (KIND(T1%K) .NE. 4 ) STOP 11
  IF (T1%K       .NE. K ) STOP 12

  IF (KIND(T1%L) .NE. K ) STOP 13
  IF (T1%L       .NE. K ) STOP 14

  IF (KIND(T1%Arr) .NE. T1%K )           STOP 15
  IF (ANY(T1%Arr   .NE. -T1%K) )         STOP 16
  IF (ANY(SHAPE(T1%Arr) .NE. (/T1%L/)) ) STOP 17


  IF (KIND(T2%K) .NE. 4 ) STOP 21
  IF (T2%K       .NE. K ) STOP 22

  IF (KIND(T2%L) .NE. K ) STOP 23
  IF (T2%L       .NE. k ) STOP 24

  IF (T2%Arr1%k .NE. K )               STOP 25
  IF (ANY(SHAPE(T2%Arr1) .NE. (/T2%L2/)) ) STOP 26

  IF (T2%Arr1(K)%K        .NE. K  )          STOP 27
  IF (T2%Arr1(K)%L        .NE. K  )          STOP 28
  IF (ANY(T2%Arr1(K)%Arr  .NE. -K) )         STOP 29




  END


