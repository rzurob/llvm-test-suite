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
!*    The associate selector is the same as a namelist name
!*   ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM AssocNameAsOther91
  NAMELIST /NL/i,j,k

  ASSOCIATE ( NL => NL )
  END ASSOCIATE



  END

