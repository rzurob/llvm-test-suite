! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 01, 2005
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
!*    The associate selector is the same as a type name
!*   (300666-confusing err msg)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM AssocNameAsOther10
  IMPLICIT NONE

  TYPE :: DT
    INTEGER :: i
  END TYPE

  INTEGER :: i
  COMMON /Cmblk/ i

  ASSOCIATE ( DT => i )
  END ASSOCIATE

  ASSOCIATE ( A => DT )
  END ASSOCIATE

  ASSOCIATE ( A => AssocNameAsOther10 )
  END ASSOCIATE

  ASSOCIATE ( A => Sub )
  END ASSOCIATE

  ASSOCIATE ( A => Cmblk )
  END ASSOCIATE

  CONTAINS
  SUBROUTINE Sub()
  END SUBROUTINE

  END

