! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 16, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED :
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
!*  ICE-TYPE GUARD outside of select type construct
!*  (298437)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM Misc16

  type :: DT
  end type
  ASSOCIATE (V=>10)
    TYPE IS (dt)
  END ASSOCIATE

  END


