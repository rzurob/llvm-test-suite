!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpEquiv6
!*
!*  DATE                       : Jul. 06, 2007
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
!*   Nagative parameter values and array component bound
!*
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT_R(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    REAL          :: R(L:0)=K
  END TYPE

  TYPE :: DT_C(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    CHARACTER(L)  :: C(L:0)=CHAR(48+K)
  END TYPE

  TYPE :: DT_I(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    INTEGER       :: I(L:0)=K
  END TYPE

  TYPE :: DT_L(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    LOGICAL       :: A(L:0)=.TRUE.
  END TYPE

  TYPE :: DT_Z(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    COMPLEX       :: Z(L:0)=(K,-K)
  END TYPE

  END MODULE

  PROGRAM dtpEquiv6
  USE M

  TYPE(DT_R(-16,-7)) R
  TYPE(DT_C(-1,-5))  C
  TYPE(DT_I(-2,-7))  I
  TYPE(DT_L(-8,-2))  L
  TYPE(DT_Z(-4,-9))  Z


  REAL(16)          :: R1(8)
  CHARACTER(5)      :: C1(6)
  INTEGER(2)        :: I1(8)
  LOGICAL(8)        :: L1(3)
  COMPLEX(16)       :: Z1(10)


  EQUIVALENCE(R, R1)
  EQUIVALENCE(C, C1)
  EQUIVALENCE(I, I1)
  EQUIVALENCE(A, A1)
  EQUIVALENCE(Z, Z1)


  IF ( ANY( R%R         .NE.   -16        ) ) STOP 11
  IF ( LBOUND( R%R, 1 ) .NE.   -16          ) STOP 12
  IF ( UBOUND( R%R, 1 ) .NE.    0           ) STOP 13

  IF ( ANY( C%C         .NE.   CHAR(48-1) ) ) STOP 21
  IF ( LBOUND( C%C, 1 ) .NE.   -5           ) STOP 22
  IF ( UBOUND( C%C, 1 ) .NE.    0           ) STOP 23

  IF ( ANY( I%I         .NE.   -2         ) ) STOP 31
  IF ( LBOUND( I%I, 1 ) .NE.   -7           ) STOP 32
  IF ( UBOUND( I%I, 1 ) .NE.    0           ) STOP 33

  IF ( ANY( L%A         .NEQV. .TRUE.     ) ) STOP 41
  IF ( LBOUND( L%A, 1 ) .NE.   -2           ) STOP 42
  IF ( UBOUND( L%A, 1 ) .NE.    0           ) STOP 43

  IF ( ANY( Z%Z         .NE.   -(-4,4)    ) ) STOP 51
  IF ( LBOUND( Z%Z, 1 ) .NE.   -9           ) STOP 52
  IF ( UBOUND( Z%Z, 1 ) .NE.    0           ) STOP 53


  R1 = 16
  C1 = CHAR(0)
  I1 = 2
  L1 = .FALSE.
  Z1 = (-4,4)


  IF ( ANY( R%R .NE.    16        ) ) STOP 61
  IF ( ANY( C%C .NE.    CHAR(0)   ) ) STOP 62
  IF ( ANY( I%I .NE.    2         ) ) STOP 63
  IF ( ANY( L%A .NEQV.  .FALSE.   ) ) STOP 64
  IF ( ANY( Z%Z .NE.    (-4,4)    ) ) STOP 65


  END


