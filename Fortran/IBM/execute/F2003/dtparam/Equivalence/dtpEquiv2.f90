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
!*  A scalar and an array are equivalenced
!*  (storage unit initialized twice, danger!)
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT_R(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    REAL(K)       :: R(L)=K
  END TYPE

  TYPE :: DT_C(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    CHARACTER(L)  :: C(L)=CHAR(48-K)
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

  PROGRAM dtpEquiv2
  USE M

  TYPE(DT_R(16,7))  :: R
  REAL(16)          :: R1(7)=-16

  TYPE(DT_C(-1,5))  :: C
  CHARACTER(5)      :: C1(5)=CHAR(48+1)

  TYPE(DT_I(2,7))   :: I
  INTEGER(2)        :: I1(7)=-2

  TYPE(DT_L(8,2))   :: L
  LOGICAL(8)        :: L1(2)=.FALSE.

  TYPE(DT_Z(16,9))  :: Z
  COMPLEX(16)       :: Z1(9)=-(16,-16)


  EQUIVALENCE(R, R1)
  EQUIVALENCE(C, C1)
  EQUIVALENCE(I, I1)
  EQUIVALENCE(l, l1)
  EQUIVALENCE(Z, Z1)

  r1 = -16
  c1 = char(49)
  i1 = -2
  l1 = .false.
  z1 = -(16,-16)

  IF ( ANY( R%R .NE.    -16         ) ) STOP 11
  R1 = 16
  IF ( ANY( R%R .NE.    16        ) ) STOP 21

  IF ( ANY( C%C .NE.    CHAR(49) ) ) STOP 12
  C1 = CHAR(0)
  IF ( ANY( C%C .NE.    CHAR(0)    ) ) STOP 22

  IF ( ANY( I%I .NE.    -2         ) ) STOP 13
  I1 = 2
  IF ( ANY( I%I .NE.    2          ) ) STOP 23

  IF ( ANY( L%A .NEQV. .FALSE.     ) ) STOP 14
  L1 = .TRUE.
  IF ( ANY( L%A .NEQV. .TRUE.      ) ) STOP 24

  IF ( ANY( Z%Z .NE.    -(16,-16)  ) ) STOP 15
  Z1 = (16,-16)
  IF ( ANY( Z%Z .NE.    (16,-16)   ) ) STOP 25

  END


