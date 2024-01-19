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
!*    The selector is an associate name which is a constant
!*    (Pass dev; print out 1 1  !??)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM C808Param3


    ASSOCIATE ( As => 1 )
      ASSOCIATE ( As1 => As)
        As1 = 2
        print*, As, As1
      END ASSOCIATE
    END ASSOCIATE

  END
