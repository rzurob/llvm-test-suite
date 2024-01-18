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
!*     Selector is an external function with the same name
!*    (Ref 293110)
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C809AssoName1

    ASSOCIATE ( As => As()  )
      IF (As .NE. 1.0) ERROR STOP 50

      ASSOCIATE ( As => As  )
        IF (As .NE. 1.0) ERROR STOP 51
      END ASSOCIATE

    END ASSOCIATE

  END

  FUNCTION As()
    As = 1.0
  END FUNCTION

