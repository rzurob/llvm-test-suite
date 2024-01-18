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
!*    The selector is an associate name associating to a constant
!*    and no redefinition of the associate name
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C808Asso
  IMPLICIT NONE

    ASSOCIATE ( As => (/1,2,3/))
      ASSOCIATE (As0 => As)
        IF ( ANY( As0 .NE. (/1,2,3/) ) ) STOP 20
        IF ( ANY( As  .NE. (/1,2,3/) ) ) STOP 21
      END ASSOCIATE
    END ASSOCIATE
  END
