! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/29/2009
!*
!*  DESCRIPTION                : defect 337052
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


  MODULE M

  TYPE :: DT0(K, L)
    INTEGER(KIND=4), KIND :: K
    INTEGER(KIND=K), LEN  :: L=K
    INTEGER(KIND=K) :: I=K
  END TYPE

  TYPE(DT0(4,128)), PARAMETER, PRIVATE :: T0=DT0(4,128)()

  TYPE, EXTENDS(DT0) :: DT1(K1, L1)
    INTEGER(KIND=T0%I), KIND :: K1=T0%K
    INTEGER(KIND=T0%I), LEN  :: L1=T0%K
    INTEGER(KIND=T0%I)       :: KK=T0%K
    INTEGER(KIND=T0%I)       :: LL(T0%L, T0%L)=  &
                                RESHAPE((/(T0%K, i=1, T0%L*T0%L)/), (/T0%L, T0%L/))
    TYPE(DT0(T0%K,  T0%L))   :: TDT0
  END TYPE

  END MODULE

  PROGRAM  dtParamTypParamDef4
  USE M

  TYPE(DT1(4, 8, K1=4, L1=8)) :: T

  IF ( T%K             .NE. 4 )             STOP 21
  IF ( KIND(T%K1)      .NE. T%K)            STOP 22
  IF ( T%K1            .NE. 4 )             STOP 23
  IF ( KIND(T%L1)      .NE. T%K )           STOP 24
  IF ( ANY(SHAPE(T%Ll) .NE. (/T%L, T%L/)))  STOP 25
  IF ( T%L1            .NE. T%K)            STOP 26
  IF ( T%TDT0%K        .NE. 4)              STOP 27
  IF ( T%TDT0%L        .NE. 8)              STOP 28

  END

