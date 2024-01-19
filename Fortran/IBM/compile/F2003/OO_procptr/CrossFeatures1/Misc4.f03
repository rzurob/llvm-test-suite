! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 29, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!* Diag
!*
!* (304368)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc4

  TYPE :: DT
    PROCEDURE() :: Ptr
  END TYPE

  END


