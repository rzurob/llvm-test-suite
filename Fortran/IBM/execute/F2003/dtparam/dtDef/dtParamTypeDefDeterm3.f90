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
!*  Determination of derived types - sequence types / allocatable
!*
!*  (Syntax err)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDefDeterm3

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

  TYPE(DT(8, 4)), TARGET  :: T0
  TYPE(DT(8, 4)), ALLOCATABLE :: T1
  TYPE(DT(8, :)), ALLOCATABLE :: T2

  T0 = DT(8,4)(-1_8, -1.0_8, (1._8, -1._8), .FALSE._8, "B")
  ALLOCATE(T1, SOURCE=T0)
  T2 = T1

  CALL Sub(T1, T2, -1_8, -1.0_8, (1._8, -1._8), .FALSE._8, "B   ")

  CONTAINS

    SUBROUTINE Sub(Arg1, Arg2, I, R, Cplx, L, C)
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

    TYPE(DT(8, *)) Arg1
    TYPE(DT(8, :)), ALLOCATABLE :: Arg2

    INTEGER(Arg1%K)  :: I
    REAL(Arg1%K)     :: R
    COMPLEX(Arg2%K)  :: Cplx
    LOGICAL(Arg2%K)  :: L
    CHARACTER(Arg1%L):: C

    IF ( Arg1%I      .NE.   I )      ERROR STOP 23
    IF ( Arg1%R      .NE.   R )      ERROR STOP 24
    IF ( Arg1%Cplx   .NE.   Cplx )   ERROR STOP 25
    IF ( Arg1%LL     .NEQV. L )      ERROR STOP 26
    IF ( TRIM(Arg1%C).NE.   TRIM(C)) ERROR STOP 27

    IF ( Arg2%I      .NE.   I )      ERROR STOP 33
    IF ( Arg2%R      .NE.   R )      ERROR STOP 34
    IF ( Arg2%Cplx   .NE.   Cplx )   ERROR STOP 35
    IF ( Arg2%LL     .NEQV. L )      ERROR STOP 36
    IF ( TRIM(Arg2%C).NE.   TRIM(C)) ERROR STOP 37

    END SUBROUTINE

  END

