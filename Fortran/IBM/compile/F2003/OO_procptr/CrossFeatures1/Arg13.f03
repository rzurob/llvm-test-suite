! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 23, 2005
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
!*  Dummy procedure - Characteristics
!*  Pointer
!*  (304112)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    INTERFACE
      FUNCTION ExtF(Arg)
        INTEGER :: Arg
        INTEGER:: ExtF
      END FUNCTION
    END INTERFACE

  END MODULE

  PROGRAM Arg13
  USE M
  IMPLICIT NONE

  CALL IntSub(ExtF)

  CONTAINS

  SUBROUTINE IntSub(Arg)
  USE M
  IMPLICIT NONE
  PROCEDURE(ExtF), POINTER :: Arg
  END SUBROUTINE

  END

  FUNCTION ExtF(Arg)
  INTEGER:: ExtF, Arg
    ExtF = Arg
  END FUNCTION
