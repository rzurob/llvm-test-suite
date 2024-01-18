!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypParamDef4
!*
!*  DATE                       : Dec. 16, 2005
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
!*  Type param def stmt - kind selector/init expr
!*
!*  (Init expr?/340245)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(K, L)
    INTEGER(KIND=4), KIND :: K
    INTEGER(KIND=K), LEN  :: L=K
    INTEGER(KIND=K) :: I=K
  END TYPE

  integer, parameter :: t0k = 4
  integer, parameter :: t0l = 128
  TYPE(DT0(t0k,t0l)), PARAMETER, PRIVATE :: T0=DT0(t0k,t0l)()

  TYPE, EXTENDS(DT0) :: DT1(K1, L1)
    INTEGER(KIND=T0k), KIND :: K1=T0K
    INTEGER(KIND=T0k), LEN  :: L1=T0K
    INTEGER(KIND=T0k)       :: KK=T0K
    INTEGER(KIND=T0k)       :: LL(T0L, T0L)=  t0k
    TYPE(DT0(T0K,  T0L))   :: TDT0
  END TYPE

  END MODULE

  PROGRAM  dtParamTypParamDef4
  USE M

  TYPE(DT1(K1=4, L1=8, L=8, K=4)) :: T

  IF ( T%K             .NE. 4 )             STOP 21
  IF ( KIND(T%K1)      .NE. T%K)            STOP 22
  IF ( T%K1            .NE. 4 )             STOP 23
  IF ( KIND(T%L1)      .NE. T%K )           STOP 24
  IF ( ANY(SHAPE(T%Ll) .NE. (/t0l, t0l/)))  STOP 25
  IF ( T%L1            .NE. 8)              STOP 26
  IF ( T%TDT0%K        .NE. 4)              STOP 27
  IF ( T%TDT0%L        .NE. t0l)            STOP 28

  END


