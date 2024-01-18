!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 13, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Determination of Types
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
!*  Determination of derived types - sequence types
!*
!*  (Syntax err)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDefDeterm2

  TYPE :: DT(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    SEQUENCE
    INTEGER(K)  :: I
    REAL(K)     :: R
    COMPLEX(K)  :: Cplx
    LOGICAL(K)  :: LL
    CHARACTER(L):: C
  END TYPE

  TYPE(DT(8, 4)) :: T0
  TYPE(DT(8, :)), POINTER :: T1

  T0 = DT(8,4)(-1_8, -1.0_8, (1._8, -1._8), .FALSE._8, "B")

  CALL Sub(T0, -1_8, -1.0_8, (1._8, -1._8), .FALSE._8, "B   ")
  CALL Sub1(T0, -1_8, -1.0_8, (1._8, -1._8), .FALSE._8, "B   ")

  ALLOCATE(T1, SOURCE=T0)

  CALL Sub1(T1, -1_8, -1.0_8, (1._8, -1._8), .FALSE._8, "B   ")
  CALL Sub2(T1, -1_8, -1.0_8, (1._8, -1._8), .FALSE._8, "B   ")

  CONTAINS

    SUBROUTINE Sub(Arg, I, R, Cplx, L, C)
    TYPE :: DT(K, L)
      INTEGER, KIND :: K
      INTEGER, LEN  :: L
      SEQUENCE
      INTEGER(K)  :: I
      REAL(K)     :: R
      COMPLEX(K)  :: Cplx
      LOGICAL(K)  :: LL
      CHARACTER(L):: C
    END TYPE

    TYPE(DT(8, 4)) Arg
    INTEGER(Arg%K)  :: I
    REAL(Arg%K)     :: R
    COMPLEX(Arg%K)  :: Cplx
    LOGICAL(Arg%K)  :: L
    CHARACTER(Arg%L):: C

    IF ( Arg%I      .NE.   I )      ERROR STOP 23
    IF ( Arg%R      .NE.   R )      ERROR STOP 24
    IF ( Arg%Cplx   .NE.   Cplx )   ERROR STOP 25
    IF ( Arg%LL     .NEQV. L )      ERROR STOP 26
    IF ( TRIM(Arg%C).NE.   TRIM(C)) ERROR STOP 27

    END SUBROUTINE

    SUBROUTINE Sub1(Arg, I, R, Cplx, L, C)
    TYPE :: DT(K, L)
      INTEGER, KIND :: K
      INTEGER, LEN  :: L
      SEQUENCE
      INTEGER(K)  :: I
      REAL(K)     :: R
      COMPLEX(K)  :: Cplx
      LOGICAL(K)  :: LL
      CHARACTER(L):: C
    END TYPE

    TYPE(DT(8, L=*)) Arg
    INTEGER(Arg%K)  :: I
    REAL(Arg%K)     :: R
    COMPLEX(Arg%K)  :: Cplx
    LOGICAL(Arg%K)  :: L
    CHARACTER(Arg%L):: C

    IF ( Arg%I      .NE.   I )      ERROR STOP 23
    IF ( Arg%R      .NE.   R )      ERROR STOP 24
    IF ( Arg%Cplx   .NE.   Cplx )   ERROR STOP 25
    IF ( Arg%LL     .NEQV. L )      ERROR STOP 26
    IF ( TRIM(Arg%C).NE.   TRIM(C)) ERROR STOP 27

    END SUBROUTINE

    SUBROUTINE Sub2(Arg, I, R, Cplx, L, C)
    TYPE :: DT(K, L)
      INTEGER, KIND :: K
      INTEGER, LEN  :: L
      SEQUENCE
      INTEGER(K)  :: I
      REAL(K)     :: R
      COMPLEX(K)  :: Cplx
      LOGICAL(K)  :: LL
      CHARACTER(L):: C
    END TYPE

    TYPE(DT(8, :)), POINTER :: Arg
    INTEGER(Arg%K)  :: I
    REAL(Arg%K)     :: R
    COMPLEX(Arg%K)  :: Cplx
    LOGICAL(Arg%K)  :: L
    CHARACTER(Arg%L):: C

    IF ( Arg%I      .NE.   I )      ERROR STOP 23
    IF ( Arg%R      .NE.   R )      ERROR STOP 24
    IF ( Arg%Cplx   .NE.   Cplx )   ERROR STOP 25
    IF ( Arg%LL     .NEQV. L )      ERROR STOP 26
    IF ( TRIM(Arg%C).NE.   TRIM(C)) ERROR STOP 27

    END SUBROUTINE

  END

