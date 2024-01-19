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
!*     The blank common block
!*
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
    SEQUENCE
    CHARACTER(L0) :: C(L0)!=CHAR(48+K0)
  END TYPE

  TYPE :: DT(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    REAL(K)       :: R(L)!=K
    CHARACTER(L)  :: C(L)!=CHAR(48+K)
    INTEGER(K)    :: I(L)!=K
    LOGICAL(K)    :: A(L)!=.TRUE.
    COMPLEX(K)    :: Z(L)!=(K,-K)
    TYPE(DT0(K,L)):: S
  END TYPE

  END MODULE

  PROGRAM dtpCommon1
  USE M

  TYPE(DT0(4,5))  :: S, S1
  TYPE(DT (8,7))  :: T, T1

  COMMON S
  COMMON //T
  COMMON S1
  COMMON //T1


  IF ( S%K0              .NE. 4         ) ERROR STOP 11
  IF ( S%L0              .NE. 5         ) ERROR STOP 12
  IF ( ANY ( LBOUND(S%C) .NE. 1       ) ) ERROR STOP 13
  IF ( ANY ( UBOUND(S%C) .NE. 5       ) ) ERROR STOP 14
  IF (       S%C%LEN     .NE. 5         ) ERROR STOP 15

  IF ( ANY ( LBOUND(T%R) .NE. 1       ) ) ERROR STOP 16
  IF ( SIZE( T%R )       .NE. 7         ) ERROR STOP 17

  IF ( ANY ( LBOUND(T%C) .NE. 1       ) ) ERROR STOP 18
  IF (       T%C%LEN     .NE. 7         ) ERROR STOP 19
  IF ( SIZE( T%C )       .NE. 7         ) ERROR STOP 20

  IF ( ANY ( LBOUND(T%I) .NE. 1       ) ) ERROR STOP 21
  IF ( SIZE( T%I )       .NE. 7         ) ERROR STOP 22

  IF ( ANY ( LBOUND(T%Z) .NE. 1       ) ) ERROR STOP 21
  IF ( SIZE( T%Z )       .NE. 7         ) ERROR STOP 22

  IF ( ANY ( LBOUND(T%A) .NE. 1       ) ) ERROR STOP 23
  IF ( SIZE( T%A )       .NE. 7         ) ERROR STOP 24

  IF ( T%S%K0              .NE. 8       ) ERROR STOP 26
  IF ( T%S%L0              .NE. 7       ) ERROR STOP 27
  IF ( ANY ( LBOUND(T%S%C) .NE. 1     ) ) ERROR STOP 28
  IF ( ANY ( UBOUND(T%S%C) .NE. 7     ) ) ERROR STOP 29
  IF (       T%S%C%LEN     .NE. 7       ) ERROR STOP 10

  S = DT0(4,5)(CHAR(0))
  S1%C  =  CHAR(5)

  T%R = -4
  T%C = CHAR(4)
  T%I = -4
  T%Z = -(4,-4)
  T%A = .TRUE.
  T%S = DT0(8,7)(CHAR(4))

  T1%R = -5
  T1%C = CHAR(5)
  T1%I = -5
  T1%Z = -(5,-5)
  T1%A = .FALSE.
  T1%S = DT0(8,7)(CHAR(5))

  CALL ExtSub()

  END

  SUBROUTINE ExtSub()
  USE M

  TYPE(DT0(4,5))  :: S, S1
  TYPE(DT (8,7))  :: T, T1

  COMMON S
  COMMON //T
  COMMON S1
  COMMON //T1


  IF ( ANY ( S%C         .NE. CHAR(0) ) ) ERROR STOP 30
  IF ( ANY ( S1%C        .NE. CHAR(5) ) ) ERROR STOP 31

  IF ( ANY ( T%R         .NE. -4      ) ) ERROR STOP 32
  IF ( ANY ( T%C         .NE. CHAR(4) ) ) ERROR STOP 33
  IF ( ANY ( T%I         .NE. -4      ) ) ERROR STOP 34
  IF ( ANY ( T%Z         .NE. -(4, -4)) ) ERROR STOP 35
  IF ( ANY ( T%A         .NEQV. .TRUE.) ) ERROR STOP 36
  IF ( ANY ( T%S%C       .NE. CHAR(4) ) ) ERROR STOP 37

  IF ( ANY ( T1%R        .NE. -5          ) ) ERROR STOP 41
  IF ( ANY ( T1%C        .NE. CHAR(5)     ) ) ERROR STOP 42
  IF ( ANY ( T1%I        .NE. -5          ) ) ERROR STOP 43
  IF ( ANY ( T1%Z        .NE. -(5, -5)    ) ) ERROR STOP 44
  IF ( ANY ( T1%A        .NEQV. .FALSE.   ) ) ERROR STOP 45
  IF ( ANY ( T1%S%C      .NE. CHAR(5)     ) ) ERROR STOP 46


  END


