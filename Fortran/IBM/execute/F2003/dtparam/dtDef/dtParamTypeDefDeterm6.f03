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
!*  (Syntax err&ice/340219)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

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

  INTERFACE
    SUBROUTINE SubIFace(Arg)
      IMPORT DT
      TYPE(DT(8, *)) Arg
    END SUBROUTINE
  END INTERFACE

  PROCEDURE(SubIFace), POINTER :: ProcPtr

  END MODULE

  PROGRAM dtParamTypeDefDeterm6
  USE M, ONLY : ProcPtr

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

  INTERFACE
    SUBROUTINE Sub(Arg)
      IMPORT
      TYPE(DT(8, *)) Arg
    END SUBROUTINE
  END INTERFACE
  TYPE(DT(8, 4))  :: T

  ProcPtr => Sub
  CALL ProcPtr(T)

  IF ( T%I      .NE.   -1_8 )           ERROR STOP 23
  IF ( T%R      .NE.   -1.0_8 )         ERROR STOP 24
  IF ( T%Cplx   .NE.   (1._8, -1._8) )  ERROR STOP 25
  IF ( T%LL     .NEQV. .FALSE._8 )      ERROR STOP 26
  IF ( TRIM(T%C).NE.   TRIM("B"))       ERROR STOP 27

  END

  SUBROUTINE Sub(Arg)
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
  TYPE(DT(8, *)) Arg

    Arg =  DT(8,4)(-1_8, -1.0_8, (1._8, -1._8), .FALSE._8, "B")

  END SUBROUTINE

