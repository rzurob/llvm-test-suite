!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpAttrSpecStmtParam3
!*
!*  DATE                       : Jun. 15, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration
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
!*  -- PARAMETER statement
!*  named constants typed implicitly shall be used consistantly to its type and type parameters
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
  END TYPE

  TYPE,  EXTENDS(DT0)  :: DT1(K1, L1)
    INTEGER(K0), KIND :: K1=1
    INTEGER(K0), LEN  :: L1=1
    INTEGER(K1)       :: I1(L1)
  END TYPE

  TYPE, EXTENDS(DT1)  :: DT2(K2,L2)
    INTEGER(K1), KIND :: K2=1
    INTEGER(K1), LEN  :: L2=1
    INTEGER(K2)       :: I2(L2)
  END TYPE

  TYPE, EXTENDS(DT2)  :: DT3(K3,L3)
    INTEGER(K2), KIND :: K3=1
    INTEGER(K2), LEN  :: L3=1
    INTEGER(K3)       :: I3(L3)
  END TYPE

  TYPE, EXTENDS(DT3)  :: DT4(K4,L4)
    INTEGER(K3), KIND :: K4=1
    INTEGER(K3), LEN  :: L4=1
    INTEGER(K4)       :: I4(L4)
  END TYPE

  TYPE, EXTENDS(DT4)  :: DT5(K5,L5)
    INTEGER(K4), KIND :: K5=1
    INTEGER(K4), LEN  :: L5=1
    INTEGER(K5)       :: I5(L5)
  END TYPE

  TYPE, EXTENDS(DT5)  :: DT6(K6,L6)
    INTEGER(K5), KIND :: K6=1
    INTEGER(K5), LEN  :: L6=1
    INTEGER(K6)       :: I6(L6)
  END TYPE

  TYPE, EXTENDS(DT6)  :: DT7(K7,L7)
    INTEGER(K6), KIND :: K7=1
    INTEGER(K6), LEN  :: L7=1
    INTEGER(K7)       :: I7(L7)
  END TYPE

  TYPE, EXTENDS(DT7)  :: DT8(K8,L8)
    INTEGER(K7), KIND :: K8=1
    INTEGER(K7), LEN  :: L8=1
    INTEGER(K8)       :: I8(L8)
  END TYPE

  INTEGER, PARAMETER  :: N=4096


  END MODULE


  PROGRAM dtpAttrSpecStmtParam3
  USE M
  IMPLICIT TYPE(DT8(K8=8,L8=8))(T)

  DIMENSION :: T(N)

  PARAMETER ( T=DT8(K0=1,L0=1,K8=8,L8=8)(      &
                 I1=1,                         &
                 I2=2,                         &
                 I3=3,                         &
                 I4=4,                         &
                 I5=5,                         &
                 I6=6,                         &
                 I7=7,                         &
                 I8=8                        ) )

  TYPE(DT8(K8=8,L8=8, K2=1, L3=1)) T


  DO I=1, N

    IF ( T(I)%L0 .NE. 1 ) STOP 11
    IF ( T(I)%L1 .NE. 1 ) STOP 12
    IF ( T(I)%L2 .NE. 1 ) STOP 13
    IF ( T(I)%L3 .NE. 1 ) STOP 14
    IF ( T(I)%L4 .NE. 1 ) STOP 15
    IF ( T(I)%L4 .NE. 1 ) STOP 16
    IF ( T(I)%L5 .NE. 1 ) STOP 17
    IF ( T(I)%L6 .NE. 1 ) STOP 18
    IF ( T(I)%L7 .NE. 1 ) STOP 21
    IF ( T(I)%L8 .NE. 8 ) STOP 22

    IF ( SIZE(T(I)%I1,1) .NE. 1  ) STOP 31
    IF ( SIZE(T(I)%I2,1) .NE. 1  ) STOP 32
    IF ( SIZE(T(I)%I3,1) .NE. 1  ) STOP 33
    IF ( SIZE(T(I)%I4,1) .NE. 1  ) STOP 34
    IF ( SIZE(T(I)%I5,1) .NE. 1  ) STOP 35
    IF ( SIZE(T(I)%I6,1) .NE. 1  ) STOP 36
    IF ( SIZE(T(I)%I7,1) .NE. 1  ) STOP 37
    IF ( SIZE(T(I)%I8,1) .NE. 8  ) STOP 38

    IF ( ANY (T(I)%I1 .NE. 1 ) ) STOP 41
    IF ( ANY (T(I)%I2 .NE. 2 ) ) STOP 42
    IF ( ANY (T(I)%I3 .NE. 3 ) ) STOP 43
    IF ( ANY (T(I)%I4 .NE. 4 ) ) STOP 44
    IF ( ANY (T(I)%I5 .NE. 5 ) ) STOP 45
    IF ( ANY (T(I)%I6 .NE. 6 ) ) STOP 46
    IF ( ANY (T(I)%I7 .NE. 7 ) ) STOP 47
    IF ( ANY (T(I)%I8 .NE. 8 ) ) STOP 48

  END DO

  END


