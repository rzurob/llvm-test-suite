! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 05, 2005
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
!*    The selector is a typeless literal
!*    (ICE-300770)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Typeless
  IMPLICIT NONE

  INTEGER(8) :: Z=81985529216486895_8
  INTEGER(4) :: O=342391_4
  INTEGER(2) :: B=170_2

  ASSOCIATE ( As => Z'0123456789ABCDEF'  )
    IF ( As .NE.  Z'0123456789ABCDEF'  )   ERROR STOP 21
    IF ( As .NE.  Z  )                     ERROR STOP 22
  END ASSOCIATE

  ASSOCIATE ( As => "01234567"O  )
    IF ( As .NE.  "01234567"O )            ERROR STOP 31
    IF ( As .NE.  O  )                     ERROR STOP 32
  END ASSOCIATE

  ASSOCIATE ( As => B"10101010"  )
    IF ( As .NE. B"10101010" )             ERROR STOP 41
    IF ( As .NE. B   )                     ERROR STOP 42
  END ASSOCIATE

  END
