! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 28, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Diagnosis on varriable deinition context
!*
!*  (ICE-298990)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM  VarDefContext2
  IMPLICIT NONE

  CONTAINS
  SUBROUTINE Sub(Arg)
   CLASS(*) ,INTENT(IN):: Arg

    SELECT TYPE (Ptr => Arg)
    CLASS DEFAULT
      STOP 40
    TYPE IS (CHARACTER(*))
      ASSOCIATE ( Ptr => Ptr)
        Ptr = "4321"
      END ASSOCIATE
    END SELECT
  END SUBROUTINE

  END
