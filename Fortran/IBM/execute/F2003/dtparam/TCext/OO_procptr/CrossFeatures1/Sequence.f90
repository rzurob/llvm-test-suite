! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/CrossFeatures1/Sequence.f
! opt variations: -ql

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

  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    SEQUENCE
    INTEGER(K1)   :: Id
    PROCEDURE(), POINTER, NOPASS :: ProcPtR
  END TYPE
  END


