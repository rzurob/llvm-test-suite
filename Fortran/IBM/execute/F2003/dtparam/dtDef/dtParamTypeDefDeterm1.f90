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
!*  Determination of derived types - from module
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: DT0(K, L)
      INTEGER, KIND :: K
      INTEGER, Len  :: L

      INTEGER(K)  :: I
      REAL(K)     :: R
      COMPLEX(K)  :: Cplx
      LOGICAL(K)  :: LL
      CHARACTER(L):: C

      CONTAINS
      PROCEDURE, PASS(Arg) :: Sub => SubDT0
    END TYPE


  CONTAINS
    SUBROUTINE SubDT0(Arg, I, R, Cplx, L, C)
    CLASS(DT0(8, *)) Arg
    INTEGER(Arg%K)  :: I
    REAL(Arg%K)     :: R
    COMPLEX(Arg%K)  :: Cplx
    LOGICAL(Arg%K)  :: L
    CHARACTER(Arg%L):: C

    IF ( Arg%I      .NE.   I )    STOP 23
    IF ( Arg%R      .NE.   R )    STOP 24
    IF ( Arg%Cplx   .NE.   Cplx ) STOP 25
    IF ( Arg%LL     .NEQV. L )    STOP 26
    IF ( TRIM(Arg%C).NE.   TRIM(C)) STOP 27

    SELECT TYPE (As => Arg)
    TYPE IS (DT0(8, *))
      PRINT*, "DT0"
    CLASS DEFAULT
      PRINT*, "Unknown Type"; STOP 11
    END SELECT

    END SUBROUTINE

  END MODULE


  PROGRAM dtParamTypeDefDeterm1
  USE M, only: dt0
  USE M, ONLY : DT1=>DT0

  TYPE(DT0(8, 4)), TARGET  :: T0
  TYPE(DT1(8, :)), POINTER :: T1

  T0 = DT0(8,4)(-1_8, -1.0_8, (1._8, -1._8), .FALSE._8, "B")
  CALL T0%Sub(-1_8, -1.0_8, (1._8, -1._8), .FALSE._8, "B    ")

  T1 => T0
  CALL T1%Sub(-1_8, -1.0_8, (1._8, -1._8), .FALSE._8, "B    ")

  END

