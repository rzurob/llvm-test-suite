! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 04, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
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
!* Select Type
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM SelectType2
  IMPLICIT CLASS(*)(U)
  TYPE :: DT
    INTEGER :: Int
    CHARACTER(30000) :: C
  END TYPE
  INTEGER :: i

  CALL Sub(DT(Int=6, C="!"))

  CONTAINS

  SUBROUTINE Sub(U)

  ASSOCIATE ( U => U)
    SELECT TYPE (U)
    CLASS IS (DT)
      ASSOCIATE (U => U)
        SELECT TYPE(U)
        CLASS DEFAULT
          ASSOCIATE ( U => U)
            IF ( U%Int .NE. 6 )       ERROR STOP 22
            IF ( TRIM(U%C) .NE. "!" ) ERROR STOP 23
          END ASSOCIATE
        END SELECT
      END ASSOCIATE

    CLASS DEFAULT
      STOP 40
    END SELECT
  END ASSOCIATE

  END SUBROUTINE

  END


