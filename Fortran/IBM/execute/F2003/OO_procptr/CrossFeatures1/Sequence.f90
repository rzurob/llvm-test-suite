! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 31, 2005
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
!* Sequence type
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Sequence

  TYPE :: DT
    SEQUENCE
    INTEGER :: Id
    PROCEDURE(), POINTER, NOPASS :: ProcPtR
  END TYPE
  END


