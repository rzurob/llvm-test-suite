!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : 338155
!*
!*  DATE                       : Jun. 26, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED CLASS PARAMETERS
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
!*  -- Namelist
!*  Add in a senario that tests illegal IO on a type with private components
!*  based on 338155
!*  (it is passing)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE, PUBLIC :: DT(L)
    INTEGER, LEN :: L=1
    CHARACTER(L), PRIVATE :: C
    CHARACTER(L)          :: C1
  END TYPE

  PRIVATE
  TYPE (DT(1)) :: T
  NAMELIST /NL/T  !OK

  CONTAINS

  SUBROUTINE S()
    print NL  ! OK
  END SUBROUTINE

  END MODULE

  MODULE M1
  USE M

  TYPE, EXTENDS(DT) :: DT1
  END TYPE

  TYPE (DT1(1)) :: T
  NAMELIST /NL/T  ! OK

  CONTAINS

  SUBROUTINE S()
    print NL  ! Illegal
  END SUBROUTINE

  END MODULE


  PROGRAM D338155

  END

