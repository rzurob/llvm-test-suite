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
!*    If selector is not a variable, associate-name
!*    shall not appear in a variable definition context
!*    (Pass comp)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM C808Param

    REAL Pi(3)
    PARAMETER (Pi=3.14)

    ASSOCIATE ( Ip => Pi )
      Ip = Pi
      print*, Ip
    END ASSOCIATE

  END
