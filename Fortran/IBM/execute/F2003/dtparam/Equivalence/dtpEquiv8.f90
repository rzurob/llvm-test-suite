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
!*  Stroage association on zero sized storage sequence
!*
!*  (338994)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT_R(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    REAL       :: R(L)=K
  END TYPE

  TYPE :: DT_C(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    CHARACTER(L)  :: C(L)=CHAR(48+K)
  END TYPE

  TYPE :: DT_I(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    INTEGER    :: I(L)=K
  END TYPE

  TYPE :: DT_L(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    LOGICAL    :: A(L)=.TRUE.
  END TYPE

  TYPE :: DT_Z(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    COMPLEX    :: Z(L)=(K,-K)
  END TYPE

  END MODULE

  PROGRAM dtpEquiv8
  USE M

  TYPE(DT_R(1,9))  :: R, R0(0)
  REAL             :: R1(9)=-1

  TYPE(DT_C(1,9))  :: C, C0(0)
  CHARACTER(9)     :: C1(9)=CHAR(48+1)

  TYPE(DT_I(1,9))  :: I, I0(0)
  INTEGER          :: I1(9)=-1

  TYPE(DT_L(1,9))  :: L, L0(0)
  LOGICAL          :: L1(9)=.FALSE.

  TYPE(DT_Z(1,9))  :: Z, Z0(0)
  COMPLEX          :: Z1(9)=-(-1,1)


  EQUIVALENCE(R, R0)
  EQUIVALENCE(R1, R0)

  EQUIVALENCE(C, C0)
  EQUIVALENCE(C0, C1)

  EQUIVALENCE(I0, I)
  EQUIVALENCE(I0, I1)

  EQUIVALENCE(l0, l1)
  EQUIVALENCE(l, l0)

  EQUIVALENCE(Z, Z0)
  EQUIVALENCE(Z0, Z1)

  r1 = -1
  c1 = char(49)
  i1 = -2
  l1 = .false.
  z1 = -(-1,1)

  IF ( ANY( R%R .NE.   -1         ) ) ERROR STOP 11
  R1 = 1
  IF ( ANY( R%R .NE.    1         ) ) ERROR STOP 21

  IF ( ANY( C%C .NE.   CHAR(48+1) ) ) ERROR STOP 12
  C1 = CHAR(0)
  IF ( ANY( C%C .NE.   CHAR(0)    ) ) ERROR STOP 22

  IF ( ANY( I%I .NE.   -2         ) ) ERROR STOP 13
  I1 = 2
  IF ( ANY( I%I .NE.    2         ) ) ERROR STOP 23

  IF ( ANY( L%A .NEQV. .FALSE.    ) ) ERROR STOP 14
  L1 = .TRUE.
  IF ( ANY( L%A .NEQV. .TRUE.     ) ) ERROR STOP 24

  IF ( ANY( Z%Z .NE.    -(-1,1)   ) ) ERROR STOP 15
  Z1 = (1,-1)
  IF ( ANY( Z%Z .NE.     (1,-1)   ) ) ERROR STOP 25

  END


