! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_procptr/CrossFeatures1/Misc4.f
! opt variations: -qnok -qnol

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

  TYPE :: DT(K1,N1)    ! (4,20)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    PROCEDURE() :: Ptr
  END TYPE

  END


