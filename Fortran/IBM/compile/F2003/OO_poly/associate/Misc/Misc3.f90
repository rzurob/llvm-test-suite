! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 02, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    Selector is a structure constructor
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM Misc3

  TYPE :: Base
     integer :: i
  END TYPE

  SELECT TYPE (as=> Base(0))
    TYPE IS (base)
  END SELECT

  END
