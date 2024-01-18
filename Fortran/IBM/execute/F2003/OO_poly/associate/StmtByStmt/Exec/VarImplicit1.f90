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
!*    The selector is a non poly implied entity of derived types
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM VarImplicit1

  TYPE :: Base
    INTEGER :: BaseId = 1
  END TYPE

  TYPE, EXTENDS(Base) :: Child
    INTEGER  :: ChildId = 2
  END TYPE

  IMPLICIT TYPE(Child)(U)

  ASSOCIATE ( As => U%BaseId )
  END ASSOCIATE

  END
