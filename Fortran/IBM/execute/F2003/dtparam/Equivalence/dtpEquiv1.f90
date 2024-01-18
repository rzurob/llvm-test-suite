!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpEquiv1 
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jul. 05, 2007
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration and specification
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   
!* 
!*  -- The equivalence statement
!* 
!*  If the equivalenced objects have differing type or type parameters, the EQUIVALENCE statement does
!*  not cause type conversion or imply mathematical equivalence.  
!*
!*  (ICE)
!*   
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
    SEQUENCE
    CHARACTER(L0) :: C(L0)=CHAR(48+K0)
  END TYPE

  TYPE :: DT(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    REAL(K)       :: R(L)=K
    CHARACTER(L)  :: C(L)=CHAR(48+K)
    INTEGER(K)    :: I(L)=K
    LOGICAL(K)    :: A(L)=.TRUE.
    COMPLEX(K)    :: Z(L)=(K,-K)
    TYPE(DT0(K,L)):: S
  END TYPE
 
  END MODULE

  PROGRAM dtpEquiv1 
  USE M

  TYPE(DT0(4,5))  :: S, S1
  TYPE(DT (8,7))  :: T, T1

  EQUIVALENCE(S, S1)
  equivalence(T, T1)
 
  IF ( S%K0              .NE. 4            ) STOP 11
  IF ( S%L0              .NE. 5            ) STOP 12
  IF ( ANY ( LBOUND(S%C) .NE. 1          ) ) STOP 13
  IF ( ANY ( UBOUND(S%C) .NE. 5          ) ) STOP 14
  IF (       S%C%LEN     .NE. 5            ) STOP 15
  IF ( ANY ( S%C         .NE. CHAR(48+4) ) ) STOP 16

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
  IF ( ANY ( LBOUND(T%Z) .NE. 1          ) ) STOP 31
  IF ( SIZE( T%Z )       .NE. 7            ) STOP 32
  IF ( ANY ( T%Z         .NE. (8, -8)    ) ) STOP 33
  IF ( ANY ( LBOUND(T%A) .NE. 1          ) ) STOP 34
  IF ( SIZE( T%A )       .NE. 7            ) STOP 35
  IF ( ANY ( T%A         .NEQV. .TRUE.   ) ) STOP 36
 
  IF ( T%S%K0              .NE. 8            ) STOP 41
  IF ( T%S%L0              .NE. 7            ) STOP 42
  IF ( ANY ( LBOUND(T%S%C) .NE. 1          ) ) STOP 43
  IF ( ANY ( UBOUND(T%S%C) .NE. 7          ) ) STOP 44
  IF (       T%S%C%LEN     .NE. 7            ) STOP 45
  IF ( ANY ( T%S%C         .NE. CHAR(48+8) ) ) STOP 46

  S = DT0(4,5)(CHAR(0))
  
  T%R = -T%R 
  T%C = CHAR(0)
  T%I = -T%I
  T%Z = -T%Z
  T%A = .NOT. T%A
  T%S = DT0(8,7)(CHAR(0)) 
 
  IF ( S1%K0              .NE. 4            ) STOP 11
  IF ( S1%L0              .NE. 5            ) STOP 12
  IF ( ANY ( LBOUND(S1%C) .NE. 1          ) ) STOP 13
  IF ( ANY ( UBOUND(S1%C) .NE. 5          ) ) STOP 14
  IF (       S1%C%LEN     .NE. 5            ) STOP 15
  IF ( ANY ( S1%C         .NE. CHAR(0)    ) ) STOP 16

  IF ( ANY ( LBOUND(T1%R) .NE. 1          ) ) STOP 20
  IF ( SIZE( T1%R )       .NE. 7            ) STOP 21
  IF ( ANY ( T1%R         .NE. -8         ) ) STOP 22
  IF ( ANY ( LBOUND(T1%C) .NE. 1          ) ) STOP 23
  IF (       T1%C%LEN     .NE. 7            ) STOP 24
  IF ( SIZE( T1%C )       .NE. 7            ) STOP 25
  IF ( ANY ( T1%C         .NE. CHAR(0)    ) ) STOP 26
  IF ( ANY ( LBOUND(T1%I) .NE. 1          ) ) STOP 27
  IF ( SIZE( T1%I )       .NE. 7            ) STOP 28
  IF ( ANY ( T1%I         .NE. -8         ) ) STOP 29
  IF ( ANY ( LBOUND(T1%Z) .NE. 1          ) ) STOP 31
  IF ( SIZE( T1%Z )       .NE. 7            ) STOP 32
  IF ( ANY ( T1%Z         .NE. -(8, -8)   ) ) STOP 33
  IF ( ANY ( LBOUND(T1%A) .NE. 1          ) ) STOP 34
  IF ( SIZE( T1%A )       .NE. 7            ) STOP 35
  IF ( ANY ( T1%A         .NEQV. .FALSE.  ) ) STOP 36
 
  IF ( T1%S%K0              .NE. 8            ) STOP 41
  IF ( T1%S%L0              .NE. 7            ) STOP 42
  IF ( ANY ( LBOUND(T1%S%C) .NE. 1          ) ) STOP 43
  IF ( ANY ( UBOUND(T1%S%C) .NE. 7          ) ) STOP 44
  IF (       T1%S%C%LEN     .NE. 7            ) STOP 45
  IF ( ANY ( T1%S%C         .NE. CHAR(0)    ) ) STOP 46

 
  END


