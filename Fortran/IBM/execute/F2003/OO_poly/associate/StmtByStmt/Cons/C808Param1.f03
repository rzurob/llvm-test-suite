!*********************************************************************
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
!*    The selector is a string literal
!*    (Seg fault)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM C808Param1

   ASSOCIATE ( As => "try!" )
     PRINT*, As
     IF ( As .NE. "try!" ) ERROR STOP 10
   END ASSOCIATE
   PRINT*, 'OK!'

  END