!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 10, 2007
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
!*  Dummy procedure and statement function
!*  ()
!   JX: 20090108: the original test case is insane: 1.) in a statement function,
!   the scalar-expr should not be structure constructor; 2.) statement functions
!   are an obsolenscent feature and their use is not encouraged.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    SEQUENCE
    INTEGER(K)    :: I=K
    CHARACTER(L)  :: C=CHAR(K)
  END TYPE


  CONTAINS

  FUNCTION ModFun(Arg1, Arg2)
  INTEGER :: Arg1
  CHARACTER(*) :: Arg2

  TYPE(DT(8,LEN(Arg2))) :: ModFun

  ModFun = DT(8,LEN(Arg2))(Arg1, Arg2)

  END FUNCTION

  SUBROUTINE ModSub(Proc)
  PROCEDURE (ModFun) :: Proc
  TYPE(DT(8,2)) T

  T = Proc(-1, "12")

  IF ( T%I             .NE.  -1            ) STOP 12
  IF ( T%C             .NE.  "12"          ) STOP 13


  END SUBROUTINE

  END MODULE

  PROGRAM  dtParamTypeDecC521_1
  USE M

  TYPE(DT(8,4)) :: T, StmtFun
  INTEGER :: L
  CHARACTER(4) :: C
!  StmtFun(L,C) = DT(8,LEN(C))(L, C)

  T = DT(8,LEN(C))(8,"ABCD")
  IF ( T%I             .NE.  8            ) STOP 12
  IF ( T%C             .NE.  "ABCD"       ) STOP 13

  CALL ModSub(ModFun)

  END
