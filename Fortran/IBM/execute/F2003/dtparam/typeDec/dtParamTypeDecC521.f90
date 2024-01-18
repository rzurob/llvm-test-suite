!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 09, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration
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
!*  C521 (R504) The function-name shall be the name of an external function,
!*  an intrinsic function, a function dummy procedure, or a statement function.
!*
!*  (336650/339281)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  FUNCTION ExtFun(Arg1, Arg2)
  INTEGER :: Arg1
  CHARACTER(*) :: Arg2

  TYPE :: DT(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    SEQUENCE
    INTEGER(K)    :: I=K
    CHARACTER(L)  :: C=CHAR(K)
  END TYPE

  TYPE(DT(8,LEN(Arg2))) :: ExtFun

  ExtFun = DT(8,LEN(Arg2))(Arg1, Arg2)

  END FUNCTION

  PROGRAM dtParamTypeDecC521

  TYPE :: DT(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    SEQUENCE
    INTEGER(K)    :: I=K
    CHARACTER(L)  :: C=CHAR(K)
  END TYPE

  INTERFACE
    FUNCTION ExtFun(Arg1, Arg2)
      INTEGER :: Arg1
      CHARACTER(*) :: Arg2

      TYPE :: DT(K,L)
        INTEGER, KIND :: K=4
        INTEGER, LEN  :: L=1
        SEQUENCE
        INTEGER(K)    :: I=K
        CHARACTER(L)  :: C=CHAR(K)
      END TYPE

      TYPE(DT(8,LEN(Arg2))) :: ExtFun
    END FUNCTION
  END INTERFACE

  TYPE(DT(8,2)) T

  T = ExtFun(-1, "12")

  IF ( T%I             .NE.  -1            ) STOP 11
  IF ( T%C             .NE.  "12"          ) STOP 12


  END

