!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jul. 11, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration and specification
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
!*  -- The common statement
!*
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
    SEQUENCE
  END TYPE

  TYPE :: DT(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    REAL(K)      :: R(L)!=K
    CHARACTER(L) :: C(L)!=CHAR(48+K)
    INTEGER(K)   :: I(L)!=K
    TYPE(DT0(K,L)):: S
  END TYPE

  END MODULE

  PROGRAM dtpCommon
  USE M

  TYPE(DT0(1,3)) :: R, R1
  TYPE(DT (8,7)) :: T, T1

  COMMON   R
  COMMON //T

  COMMON /BLK/R1
  COMMON /BLK/T1

  IF ( R%K0  .NE. 1   ) STOP 11
  IF ( R%L0  .NE. 3   ) STOP 12

  IF ( R1%K0 .NE. 1   ) STOP 13
  IF ( R1%L0 .NE. 3   ) STOP 14

  IF ( ANY ( LBOUND(T%R) .NE. 1          ) ) STOP 20
  IF ( SIZE( T%R )       .NE. 7            ) STOP 21

  IF ( ANY ( LBOUND(T%C) .NE. 1          ) ) STOP 23
  IF (       T%C%LEN     .NE. 7            ) STOP 24
  IF ( SIZE( T%C )       .NE. 7            ) STOP 25

  IF ( ANY ( LBOUND(T%I) .NE. 1          ) ) STOP 27
  IF ( SIZE( T%I )       .NE. 7            ) STOP 28

  IF ( T%S%K0 .NE. 8   ) STOP 41
  IF ( T%S%L0 .NE. 7   ) STOP 42

  IF ( ANY ( LBOUND(T1%R) .NE. 1          ) ) STOP 30
  IF ( SIZE( T1%R )       .NE. 7            ) STOP 31

  IF ( ANY ( LBOUND(T1%C) .NE. 1          ) ) STOP 33
  IF (       T1%C%LEN     .NE. 7            ) STOP 34
  IF ( SIZE( T1%C )       .NE. 7            ) STOP 35

  IF ( ANY ( LBOUND(T1%I) .NE. 1          ) ) STOP 37
  IF ( SIZE( T1%I )       .NE. 7            ) STOP 38

  IF ( T1%S%K0 .NE. 8   ) STOP 43
  IF ( T1%S%L0 .NE. 7   ) STOP 44

  T%R = -8
  T%C = CHAR(0)
  T%I = -7

  T1%R = -8
  T1%C = CHAR(0)
  T1%I = -7

  CALL ExtSub()

  END

  SUBROUTINE ExtSub()
  USE M

  TYPE(DT (8,7)) :: T, T1

  COMMON //T
  COMMON /BLK/T1

  IF ( ANY ( LBOUND(T%R) .NE. 1          ) ) STOP 50
  IF ( SIZE( T%R )       .NE. 7            ) STOP 51
  IF ( ANY ( T%R         .NE. -8         ) ) STOP 52
  IF ( ANY ( LBOUND(T%C) .NE. 1          ) ) STOP 53
  IF (       T%C%LEN     .NE. 7            ) STOP 54
  IF ( SIZE( T%C )       .NE. 7            ) STOP 55
  IF ( ANY ( T%C         .NE. CHAR(0)    ) ) STOP 56
  IF ( ANY ( LBOUND(T%I) .NE. 1          ) ) STOP 57
  IF ( SIZE( T%I )       .NE. 7            ) STOP 58
  IF ( ANY ( T%I         .NE. -7         ) ) STOP 59

  IF ( ANY ( LBOUND(T1%R) .NE. 1          ) ) STOP 60
  IF ( SIZE( T1%R )       .NE. 7            ) STOP 61
  IF ( ANY ( T1%R         .NE. -8         ) ) STOP 62
  IF ( ANY ( LBOUND(T1%C) .NE. 1          ) ) STOP 63
  IF (       T1%C%LEN     .NE. 7            ) STOP 64
  IF ( SIZE( T1%C )       .NE. 7            ) STOP 65
  IF ( ANY ( T1%C         .NE. CHAR(0)    ) ) STOP 66
  IF ( ANY ( LBOUND(T1%I) .NE. 1          ) ) STOP 67
  IF ( SIZE( T1%I )       .NE. 7            ) STOP 68
  IF ( ANY ( T1%I         .NE. -7         ) ) STOP 69

  END SUBROUTINE


