! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 2, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C811
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
!*    In the nested associate and select type constructs, The selector is
!*    a poly used as string section(Diagnostic)
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM  Misc1
  IMPLICIT NONE

  CLASS(*), POINTER :: Ptr

  ALLOCATE(Ptr, SOURCE="1234567890")

  ASSOCIATE ( As => Ptr)
  SELECT TYPE (  As(:) )
    TYPE IS (CHARACTER(*))
      STOP 20
    CLASS DEFAULT
      STOP 30
  END SELECT
  END ASSOCIATE
  STOP 40

  END

