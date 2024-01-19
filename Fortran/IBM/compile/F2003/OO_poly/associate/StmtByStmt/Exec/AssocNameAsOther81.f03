! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 01, 2005
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
!*    The selector is the same as a structure name
!*   (ICE-300556)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM AssocNameAsOther81

A1:  ASSOCIATE ( A1 => A1)
  END ASSOCIATE A1

  END

