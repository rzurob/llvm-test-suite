!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jul. 05, 2007
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
!*  -- The equivalence statement
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
    REAL(K)      :: R(L)=K
    CHARACTER(L) :: C(L)=CHAR(48+K)
    INTEGER(K)   :: I(L)=K
    TYPE(DT0(K,L)):: S
  END TYPE

  END MODULE

  PROGRAM dtpEquiv
  USE M

  TYPE(DT0(1,3)) :: R, R1
  TYPE(DT (8,7)) :: T, T1

  EQUIVALENCE(R, R1)
  EQUIVALENCE(T, T1)

  IF ( R%K0  .NE. 1   ) STOP 11
  IF ( R%L0  .NE. 3   ) STOP 12
  IF ( R1%K0 .NE. 1   ) STOP 13
  IF ( R1%L0 .NE. 3   ) STOP 14

  IF ( ANY ( LBOUND(T%R) .NE. 1          ) ) STOP 20
  IF ( SIZE( T%R )       .NE. 7            ) STOP 21
  IF ( ANY ( T%R         .NE. 8          ) ) STOP 22
  IF ( ANY ( LBOUND(T%C) .NE. 1          ) ) STOP 23
  IF (       T%C%LEN     .NE. 7            ) STOP 24
  IF ( SIZE( T%C )       .NE. 7            ) STOP 25
  IF ( ANY ( T%C         .NE. CHAR(48+8) ) ) STOP 26
  IF ( ANY ( LBOUND(T%I) .NE. 1          ) ) STOP 27
  IF ( SIZE( T%I )       .NE. 7            ) STOP 28
  IF ( ANY ( T%I         .NE. 8          ) ) STOP 29

  IF ( T%S%K0 .NE. 8   ) STOP 31
  IF ( T%S%L0 .NE. 7   ) STOP 32

  T%R = -T%R
  T%C = CHAR(0)
  T%I = -T%I

  IF ( ANY ( LBOUND(T1%R) .NE. 1          ) ) STOP 40
  IF ( SIZE( T1%R )       .NE. 7            ) STOP 41
  IF ( ANY ( T1%R         .NE. -8         ) ) STOP 42
  IF ( ANY ( LBOUND(T1%C) .NE. 1          ) ) STOP 43
  IF (       T1%C%LEN     .NE. 7            ) STOP 44
  IF ( SIZE( T1%C )       .NE. 7            ) STOP 45
  IF ( ANY ( T1%C         .NE. CHAR(0)    ) ) STOP 46
  IF ( ANY ( LBOUND(T1%I) .NE. 1          ) ) STOP 47
  IF ( SIZE( T1%I )       .NE. 7            ) STOP 48
  IF ( ANY ( T1%I         .NE. -8         ) ) STOP 49

  END


