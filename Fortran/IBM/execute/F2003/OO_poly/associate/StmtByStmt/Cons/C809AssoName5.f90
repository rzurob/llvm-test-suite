! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 20, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : Associate name
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
!*     The associate name must only be declared once in the ASSOCIATE statement
!*     Selector is an intrinsic with the same name as associate name
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C809AssoName5
  IMPLICIT NONE

    ASSOCIATE ( Sin => Sin(0.0)  )
      IF ( Sin .NE. 0.0) ERROR STOP 50

      ASSOCIATE ( Sin => Sin  )
        IF ( Sin .NE. 0.0) ERROR STOP 50
      END ASSOCIATE
    END ASSOCIATE

  END

