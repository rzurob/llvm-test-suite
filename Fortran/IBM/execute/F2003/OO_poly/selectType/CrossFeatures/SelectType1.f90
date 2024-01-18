! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: SelectType1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SelectCase1
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
!* (299308)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM SelectType1
  IMPLICIT CLASS(*)(U)
  TYPE :: DT
    INTEGER :: Int
    CHARACTER(3) :: C
  END TYPE
  INTEGER :: i

  CALL Sub(DT(Int=6, C="!"))

  CONTAINS

  SUBROUTINE Sub(U)

A:SELECT TYPE (U)
  CLASS IS (DT)

    ASSOCIATE (U => U)
      associate:SELECT TYPE(U)
      CLASS DEFAULT
        select: ASSOCIATE ( U => U)
          IF ( U%Int .NE. 6 )       STOP 30
          IF ( TRIM(U%C) .NE. "!" ) STOP 30
        END ASSOCIATE select
      END SELECT associate
    END ASSOCIATE

  CLASS DEFAULT
    STOP 40
  END SELECT A

  END SUBROUTINE

  END



