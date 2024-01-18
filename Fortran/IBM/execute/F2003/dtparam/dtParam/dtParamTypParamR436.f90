!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypParamR436
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
!*  - Constant
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypParamR436

  INTEGER, PARAMETER :: F=4

  TYPE DT0(K, L)
    INTEGER, KIND         :: K=F
    INTEGER(KIND=1), LEN  :: L=F
    INTEGER(KIND=K)       :: Arr(L)= -K
  END TYPE

  TYPE( DT0 ), PARAMETER :: T0=DT0()

  TYPE :: DT1(K, L)
    INTEGER(KIND=F), KIND :: K=F+F
    INTEGER(KIND=F), LEN  :: L=F+F
    INTEGER(KIND=K)       :: KK=K
    INTEGER(KIND=K)       :: LL(L, K)=K
  END TYPE

  TYPE, EXTENDS(DT0) :: DT2(K2, L2)
    INTEGER(KIND=f), KIND :: K2=f
    INTEGER(KIND=f), LEN  :: L2=f
    INTEGER(KIND=f)       :: KK=T0%Arr(1)
    INTEGER(KIND=f)       :: LL(K2, L2)=f
  END TYPE


  TYPE (DT1)    :: T1
  TYPE (DT2)    :: T2


  IF (KIND(T1%K) .NE. F   ) STOP 11
  IF (T1%K       .NE. 2*F ) STOP 12

  IF (KIND(T1%L) .NE. F   ) STOP 13
  IF (T1%L       .NE. 2*F ) STOP 14

  IF (KIND(T1%KK) .NE. T1%K ) STOP 15
  IF (T1%KK       .NE. T1%K ) STOP 16

  IF (KIND(T1%LL) .NE. T1%K )                   STOP 17
  IF (ANY(T1%LL   .NE. T1%K) )                  STOP 18
  IF (ANY(SHAPE(T1%LL)   .NE. (/T1%L, T1%K/)) ) STOP 19


  IF (KIND(T2%K2) .NE. F ) STOP 11
  IF (T2%K2       .NE. F ) STOP 12

  IF (KIND(T2%L2) .NE. F ) STOP 13
  IF (T2%L2       .NE. F ) STOP 14

  IF (KIND(T2%KK) .NE. T0%K  ) STOP 15
  IF (T2%KK       .NE. -T0%K ) STOP 16

  IF (KIND(T2%LL) .NE. T0%K )                   STOP 17
  IF (ANY(T2%LL   .NE. f) )                  STOP 18
  IF (ANY(SHAPE(T2%LL)   .NE. (/integer :: T0%L, T0%K/)) ) STOP 19

  END


