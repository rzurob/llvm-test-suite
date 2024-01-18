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
!*  Partially storage associated (storage unit initialized twice, danger!)
!*
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT_R(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    REAL(K)       :: R(L)=K/8
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
    INTEGER(K)    :: I(L)=K
  END TYPE

  TYPE :: DT_L(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    LOGICAL(K)    :: A(L)=.TRUE.
  END TYPE

  TYPE :: DT_Z(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    COMPLEX(K)    :: Z(L)=(K,-K)
  END TYPE

  END MODULE

  PROGRAM dtpEquiv3
  USE M

  TYPE(DT_R(16,7))  :: R
  REAL(16)          :: R1(2)=16

  TYPE(DT_C(1,5))  :: C
  CHARACTER(5)      :: C1(2)=CHAR(48)

  TYPE(DT_I(2,7))   :: I
  INTEGER(2)        :: I1(2)=-2

  TYPE(DT_L(8,2))   :: L
  LOGICAL(8)        :: L1(2)=.FALSE.

  TYPE(DT_Z(16,9))  :: Z
  COMPLEX(16)       :: Z1(2)=-(16,-16)


  EQUIVALENCE(R, R1)
  EQUIVALENCE(C, C1)
  EQUIVALENCE(I, I1)
  EQUIVALENCE(l, l1)
  EQUIVALENCE(Z, Z1)

  r1 = 16
  c1 = char(48)
  i1 = -2
  l1 = .false.
  z1 = -(16,-16)

  IF ( ANY( R%R(1:2) .NE.    16         ) ) STOP 11
  IF ( ANY( R%R(3: ) .NE.    2          ) ) STOP 12
  R1 = -16
  IF ( ANY( R%R(1:2) .NE.    -16        ) ) STOP 13
  IF ( ANY( R%R(3: ) .NE.    2          ) ) STOP 14


  IF ( ANY( C%C(1:2) .NE.    CHAR(48  ) ) ) STOP 21
  IF ( ANY( C%C(3: ) .NE.    CHAR(48+1) ) ) STOP 22
  C1 = CHAR(0)
  IF ( ANY( C%C(1:2) .NE.    CHAR(0   ) ) ) STOP 23
  IF ( ANY( C%C(3: ) .NE.    CHAR(48+1) ) ) STOP 24


  IF ( ANY( I%I(1:2) .NE.    -2         ) ) STOP 31
  IF ( ANY( I%I(3: ) .NE.     2         ) ) STOP 32
  I1 = 2
  IF ( ANY( I%I(1:2) .NE.     2         ) ) STOP 33
  IF ( ANY( I%I(3: ) .NE.     2         ) ) STOP 34


  IF ( ANY( L%A(1:2) .NEQV. .FALSE.     ) ) STOP 41
  IF ( ANY( L%A(3: ) .NEQV. .TRUE.      ) ) STOP 42
  L1 = .TRUE.
  IF ( ANY( L%A(1:2) .NEQV. .TRUE.      ) ) STOP 43
  IF ( ANY( L%A(3: ) .NEQV. .TRUE.      ) ) STOP 44

  IF ( ANY( Z%Z(1:2) .NE.    -(16,-16)  ) ) STOP 51
  IF ( ANY( Z%Z(3: ) .NE.     (16,-16)  ) ) STOP 52
  Z1 = (16,-163)
  IF ( ANY( Z%Z(1:2) .NE.     (16,-163)  ) ) STOP 53
  IF ( ANY( Z%Z(3: ) .NE.     (16,-16)  ) ) STOP 54


  END


