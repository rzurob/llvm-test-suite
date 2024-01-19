!*********************************************************************
!*  ===================================================================
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
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(L)
    INTEGER, LEN :: L=1
    CHARACTER(L) :: C
  END TYPE

  TYPE (DT(1)) :: T

  END MODULE

  MODULE M1
  USE M

  NAMELIST /NL/T  ! illegal
  NAMELIST /NL1/T ! legal

  PRIVATE T,NL1

  END MODULE

  PROGRAM D338155

  END


