!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 15, 2005
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
!*  (Syntax err&ice)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: SeqType(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    SEQUENCE
    INTEGER(K)  :: I
    REAL(K)     :: R
    COMPLEX(K)  :: Cplx
    LOGICAL(K)  :: LL
    CHARACTER(L):: C
  END TYPE

  TYPE(SeqType(8, 4)), TARGET      :: T0
  TYPE(SeqType(8, :)), ALLOCATABLE :: T1
  TYPE(SeqType(8, :)), POINTER     :: T2

  END MODULE

  PROGRAM dtParamTypeDefDeterm4
  USE M, DT => SeqType

  CALL S()

  CONTAINS
  SUBROUTINE S()

  TYPE :: SeqType(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    SEQUENCE
    INTEGER(K)  :: I
    REAL(K)     :: R
    COMPLEX(K)  :: Cplx
    LOGICAL(K)  :: LL
    CHARACTER(L):: C
  END TYPE

  T0 = SeqType(8,4)(-1_8, -1.0_8, (1._8, -1._8), .FALSE._8, "B")

  IF ( T0%I      .NE.   -1_8 )           STOP 23
  IF ( T0%R      .NE.   -1.0_8 )         STOP 24
  IF ( T0%Cplx   .NE.   (1._8, -1._8) )  STOP 25
  IF ( T0%LL     .NEQV. .FALSE._8 )      STOP 26
  IF ( TRIM(T0%C).NE.   TRIM("B"))       STOP 27

  ALLOCATE(DT(8,4) :: T1)
  T1 = T0

  IF ( T1%I      .NE.   -1_8 )           STOP 33
  IF ( T1%R      .NE.   -1.0_8 )         STOP 34
  IF ( T1%Cplx   .NE.   (1._8, -1._8) )  STOP 35
  IF ( T1%LL     .NEQV. .FALSE._8 )      STOP 36
  IF ( TRIM(T1%C).NE.   TRIM("B"))       STOP 37

  T2 => T0

  IF ( T2%I      .NE.   -1_8 )           STOP 43
  IF ( T2%R      .NE.   -1.0_8 )         STOP 44
  IF ( T2%Cplx   .NE.   (1._8, -1._8) )  STOP 45
  IF ( T2%LL     .NEQV. .FALSE._8 )      STOP 46
  IF ( TRIM(T2%C).NE.   TRIM("B"))       STOP 47

  END SUBROUTINE

  END

