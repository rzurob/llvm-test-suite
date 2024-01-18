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
!*    This TC tests if an associate name can be redefined
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER :: BaseId = 1
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
      INTEGER, ALLOCATABLE :: IntArr(:)
    END TYPE

  END MODULE

  PROGRAM HostAssocStruct1
  USE M
  IMPLICIT NONE

  ASSOCIATE (T =>  Child(BaseId=-1, ChildId=-2, IntArr=(/-1,-2,-3/)))
  ASSOCIATE (As0  => T )
    As0 =  Child(BaseId=-1, ChildId=-2, IntArr=(/-1,-2,-3/))
  END ASSOCIATE
  END ASSOCIATE


  END
