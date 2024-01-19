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
!*  array constructor
!* (ICE : 296205 )
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc16
  IMPLICIT NONE

  TYPE :: BASE
    INTEGER :: BaseID = 2
  END TYPE

  CLASS(*), ALLOCATABLE :: Var

  ALLOCATE(Base:: Var)

  ASSOCIATE ( As => (/Var/) )
  END ASSOCIATE

 END

