! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 12, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  C726 (R742) An expr shall be a reference to a function whose result
!*  is a procedure pointer.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM PrtAssignTarExpr3
  IMPLICIT NONE

  INTEGER,          TARGET :: ITar
  REAL,             TARGET :: RTar
  CHARACTER,        TARGET :: CTar
  DOUBLE PRECISION, TARGET :: DTar
  LOGICAL,          TARGET :: LTar
  BYTE,             TARGET :: BTar

  PROCEDURE(INTEGER),          POINTER :: IPtr
  PROCEDURE(REAL),             POINTER :: RPtr
  PROCEDURE(CHARACTER),        POINTER :: CPtr
  PROCEDURE(DOUBLE PRECISION), POINTER :: DPtr
  PROCEDURE(LOGICAL),          POINTER :: LPtr
  PROCEDURE(Byte),             POINTER :: BPtr

    IPtr  => ITar

    RPtr  => RTar

    CPtr  => CTar

    DPtr  => DTar

    LPtr  => LTar

    BPtr  => BTar

  END

