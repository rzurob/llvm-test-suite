! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 26, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : Selector is a constant
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
!*    The selector is a wrong array section
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C808Misc
  IMPLICIT NONE
  CHARACTER :: Arr(10) = "k"

    ASSOCIATE ( As => Arr(:: ))
    END ASSOCIATE

  END
