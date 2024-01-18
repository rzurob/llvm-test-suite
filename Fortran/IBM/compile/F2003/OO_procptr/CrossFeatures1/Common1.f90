! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 28, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED :
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
!*  Common block - diaignosis on C588
!*  (304771)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 MODULE M
 USE ISO_C_BINDING, ONLY: C_INT

  INTERFACE
    FUNCTION CF() BIND(C)
    IMPORT
      INTEGER(C_INT) :: CF
    END FUNCTION
  END INTERFACE

 END MODULE

  PROGRAM Common1
  USE M

  PROCEDURE(CF), POINTER, BIND(C) :: ProcPtr
  COMMON ProcPtr

  PROCEDURE(CF)          :: ExtFun
  COMMON ExtFun


  CONTAINS

  SUBROUTINE IntSub(ProcPtr)
  PROCEDURE(CF), POINTER,  BIND(C) :: ProcPtr
  COMMON ProcPtr
  END SUBROUTINE


  END


