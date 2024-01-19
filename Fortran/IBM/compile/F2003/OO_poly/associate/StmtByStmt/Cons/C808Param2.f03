! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 20, 2004
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
!*    The selector is a string constant
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM C808Param2

    CHARACTER(10) :: Hello
    PARAMETER (Hello="Hello")

    print*, Hello
    ASSOCIATE ( As => Hello )
      As = "changed!"
      print*, As
    END ASSOCIATE

  END
