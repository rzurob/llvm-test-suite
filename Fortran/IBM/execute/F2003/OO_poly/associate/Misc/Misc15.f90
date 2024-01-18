! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 02, 2004
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
!*  array of character
!* (ICE : 294575 )
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc15

  CHARACTER :: Arr(10) = "k"
  print*,  Arr(1) ,  Arr(::1)
  ASSOCIATE ( As => Arr(10:2:-2 ))
  END ASSOCIATE

  END

