! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 2, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C811
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
!*   Non named var as selector
!*  (diagnostic)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM  Misc3
  IMPLICIT NONE

  TYPE:: Base
    CLASS(*), POINTER :: ul
  END TYPE

  TYPE(BASE) :: V

  ALLOCATE(V%ul, SOURCE=1_4)

  SELECT TYPE (V%ul)

  END SELECT

  END

