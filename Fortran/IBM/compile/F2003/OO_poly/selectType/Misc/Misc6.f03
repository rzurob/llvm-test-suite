! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 16, 2004
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
!*  Non poly entity of implicit type
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
    END TYPE

    CONTAINS

    SUBROUTINE Sub(CArg)
    IMPLICIT TYPE(DT)(C)

      SELECT TYPE (CArg)
        TYPE IS(DT)
      END SELECT
    END SUBROUTINE

  END MODULE

  PROGRAM  Misc6
  END

