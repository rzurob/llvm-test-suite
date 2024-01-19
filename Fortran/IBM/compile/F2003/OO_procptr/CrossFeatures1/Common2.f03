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
!*  Common block -
!*  A variable-name or proc-pointer-name shall not be a name made accessible
!*  by use association.
!*  (304369-invalid. Overwriting the host ones)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  PROCEDURE(), POINTER :: ProcPtr
  INTEGER              :: I
  COMMON ProcPtr


  END MODULE

  PROGRAM Common2
  USE M

  COMMON ProcPtr

  CONTAINS

  SUBROUTINE IntSub()
  COMMON ProcPtr  !no complain here
  END SUBROUTINE


  END

  SUBROUTINE ExtSub()
  USE M
  COMMON ProcPtr

  CONTAINS

  SUBROUTINE IntSub()
  COMMON ProcPtr  !no complain here
  END SUBROUTINE
  END SUBROUTINE

  MODULE M1
  USE M
    COMMON ProcPtr

    1=1  ! Make compilation fail

  END MODULE

