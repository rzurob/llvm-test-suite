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
!*  Array element designators
!*
!*  ()
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

  PROGRAM dtpEquiv9
  USE M

  TYPE(DT_R(1,9))  :: R(9)
  REAL             :: R1(9)=-1

  TYPE(DT_C(1,9))  :: C(9)
  CHARACTER(9)     :: C1(9)=CHAR(48-1)

  TYPE(DT_I(2,9))  :: I(9)
  INTEGER          :: I1(9)=-1

  TYPE(DT_L(1,9))  :: L(9)
  LOGICAL          :: L1(9)=.FALSE.

  TYPE(DT_Z(1,9))  :: Z(9)
  COMPLEX          :: Z1(9)=-(-1,1)


  EQUIVALENCE(R(1), R1)

  EQUIVALENCE(C(5), C1)

  EQUIVALENCE(I(9), I1)

  EQUIVALENCE(L(2), L1)

  EQUIVALENCE(Z(8), Z1)

  r1 = -1
  i1 = -1
  l1 = .false.
  z1 = -(-1,1)
  c1 = char(47)

  DO J=2, 8
    IF ( ANY( R(J)%R .NE.   1         ) ) STOP 11
  END DO
    IF ( ANY( R(1)%R .NE.   -1        ) ) STOP 12

  R1 = 2

  DO J=2, 8
    IF ( ANY( R(J)%R .NE.   1         ) ) STOP 13
  END DO
    IF ( ANY( R(1)%R .NE.   2         ) ) STOP 14


  DO J=1, 9
    IF ( J .NE. 5) THEN
      IF ( ANY( C(J)%C .NE. CHAR(48+1)  ) ) STOP 21
    ELSE
      IF ( ANY( C(J)%C .NE. CHAR(48-1)  ) ) STOP 22
    END IF
  END DO

  C1 = CHAR(0)

  DO J=1, 9
    IF ( J .NE. 5) THEN
      IF ( ANY( C(J)%C .NE. CHAR(48+1)  ) ) STOP 23
    ELSE
      IF ( ANY( C(J)%C .NE. CHAR(0)     ) ) STOP 24
    END IF
  END DO


  DO J=1, 9
    IF ( J .NE. 9) THEN
      IF ( ANY( I(J)%I .NE. 2  ) ) STOP 31
    ELSE
      IF ( ANY( I(J)%I .NE. -1 ) ) STOP 32
    END IF
  END DO

  I1 = -2

  DO J=1, 9
    IF ( J .NE. 9) THEN
      IF ( ANY( I(J)%I .NE. 2  ) ) STOP 33
    ELSE
      IF ( ANY( I(J)%I .NE. -2 ) ) STOP 34
    END IF
  END DO


  DO J=1, 9
    IF ( J .NE. 2) THEN
      IF ( ANY( L(J)%A .NEQV. .TRUE.  ) ) STOP 41
    ELSE
      IF ( ANY( L(J)%A .NEQV. .FALSE. ) ) STOP 42
    END IF
  END DO

  L1 = .TRUE.

  DO J=1, 9
    IF ( J .NE. 2) THEN
      IF ( ANY( L(J)%A .NEQV. .TRUE.  ) ) STOP 43
    ELSE
      IF ( ANY( L(J)%A .NEQV. .TRUE.  ) ) STOP 44
    END IF
  END DO


  DO J=1, 9
    IF ( J .NE. 8) THEN
      IF ( ANY( Z(J)%Z .NE. (1,-1)  ) ) STOP 51
    ELSE
      IF ( ANY( Z(J)%Z .NE. (1,-1) ) ) STOP 52
    END IF
  END DO

  Z1 = (2,-2)

  DO J=1, 9
    IF ( J .NE. 8) THEN
      IF ( ANY( Z(J)%Z .NE. (1,-1)  ) ) STOP 53
    ELSE
      IF ( ANY( Z(J)%Z .NE. (2,-2)  ) ) STOP 54
    END IF
  END DO


  END


