! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 20, 2005
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
!*  associate entity's scope
!*
!* (298502)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc21

  ASSOCIATE ( P1 => P1())
    IF ( P1 .NE. 1. ) ERROR STOP 111
  END ASSOCIATE
  END

  real function P1()
    p1 = 1.
  end function


