! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 28, 2005
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
!*    The associate construct name is the same as external entity's name
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM AssocNameAsOther2

  INTEGER(8), EXTERNAL :: Ext


  ASSOCIATE ( Ext => Ext(1)  )
    IF ( Ext .NE. 1_8 ) Stop 11
  END ASSOCIATE
  IF ( Ext(1) .NE. 1_8 ) Stop 12

  CALL Sub(Ext)

  CONTAINS

  SUBROUTINE Sub(Arg)
  PROCEDURE(INTEGER(8)) :: Arg

    ASSOCIATE ( Arg => Arg(1) )
      IF ( Arg .NE. 1_8 ) Stop 13
    END ASSOCIATE

  END SUBROUTINE

  END

  INTEGER(8) FUNCTION Ext(Arg)
  INTEGER :: Arg
    Ext = Arg
  END FUNCTION


