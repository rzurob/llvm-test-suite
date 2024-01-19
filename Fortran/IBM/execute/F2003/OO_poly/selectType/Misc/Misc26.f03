! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 03, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Finalization
!*  (ICE-299471)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE  :: DT
      CONTAINS
      Final :: FinalDT
    END TYPE

    CONTAINS

    SUBROUTINE FinalDT(Arg)
    TYPE(DT) :: Arg
      PRINT*, "Final DT"
    END SUBROUTINE
  END MODULE

  PROGRAM Misc26
  USE M
  TYPE :: Test
    TYPE(DT) :: W = DT()
  END TYPE

  print*, "Before End"

  END




