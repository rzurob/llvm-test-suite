! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 04, 2005
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
!* Inappropriate use of associating entity.
!*
!* (ICE-301478 )
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc18

  INTRINSIC SIN
    CALL Sub(SIN)

  CONTAINS
  SUBROUTINE Sub(SS)
  EXTERNAL SS
  PRINT *, SS(3.14/2.)
  ASSOCIATE ( As => SS(3.14/2.))
    PRINT*, As(3.14/2.)
  END ASSOCIATE

  END SUBROUTINE
  END


