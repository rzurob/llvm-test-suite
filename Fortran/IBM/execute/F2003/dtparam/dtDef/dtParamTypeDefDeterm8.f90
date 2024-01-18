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
!*  (ICE)
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
    SUBROUTINE IFun(Arg)
      IMPORT DT
      TYPE(DT(8, 4)) Fun
      TYPE(DT(8, *)) Arg
    END SUBROUTINE
  END INTERFACE

  TYPE :: DT1(K, L)
    INTEGER, KIND :: K
    INTEGER, LEN  :: L
    PROCEDURE(IFun), NOPASS, POINTER :: ProcPtr
  CONTAINS
    PROCEDURE, NOPASS :: Fun => ModFun
  END TYPE

  CONTAINS

  FUNCTION ModFun(Proc)
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
    FUNCTION IFun(Arg)
      IMPORT DT
      TYPE(DT(8, 4)) Fun
      TYPE(DT(8, *)) Arg
    END FUNCTION
  END INTERFACE

  PROCEDURE (IFun)          :: Proc
  PROCEDURE (IFun), POINTER :: ModFun
    ModFun => Proc
  END FUNCTION

  END MODULE

  PROGRAM dtParamTypeDefDeterm8
  USE M, ONLY : DT1

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
    FUNCTION ExtFun(Arg)
      IMPORT DT
      TYPE(DT(8, *)) Arg
      TYPE(DT(8, 4)) ExtFun
    END FUNCTION
  END INTERFACE

  TYPE(DT(8, 4))  :: T
  TYPE(DT1(8,4))  :: TT

  TT%ProcPtr => TT%Fun(ExtFun)
  T = TT%ProcPtr(DT(8,4)(-1_8, -1.0_8, (1._8, -1._8), .FALSE._8, "B"))

  IF ( T%I      .NE.   -1_8 )           STOP 23
  IF ( T%R      .NE.   -1.0_8 )         STOP 24
  IF ( T%Cplx   .NE.   (1._8, -1._8) )  STOP 25
  IF ( T%LL     .NEQV. .FALSE._8 )      STOP 26
  IF ( TRIM(T%C).NE.   TRIM("B"))       STOP 27

  END

  FUNCTION ExtFun(Arg)
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
  TYPE(DT(8, 4)) ExtFun

    ExtFun = Arg

  END FUNCTION

