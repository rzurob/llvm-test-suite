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
!*    The selector is an array with vector subscript
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C808ArrVec
  IMPLICIT NONE
  CHARACTER :: Arr(10)
  INTEGER   :: i,Script(3) = (/1, 3, 7/)

    ASSOCIATE ( As => Arr((/1/)))
      As = "2"
      PRINT*, As
    END ASSOCIATE

    ASSOCIATE ( As => Arr(Script) )
      As = As
    END ASSOCIATE

    ASSOCIATE ( As => Arr( (/(i, i=1, 10, 2)/)) )
      As = "!"
    END ASSOCIATE


  END