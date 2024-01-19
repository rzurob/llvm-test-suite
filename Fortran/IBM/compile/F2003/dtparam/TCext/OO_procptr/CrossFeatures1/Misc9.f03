! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/CrossFeatures1/Misc9.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 08, 2005
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
!*  Objects with proc-ptr components are not allowed in default IO.
!*
!*  (ICE-304882)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc9
  IMPLICIT NONE

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: I=1
    PROCEDURE(), POINTER, NOPASS :: ProcPtr => NULL()
  END TYPE

  TYPE :: DT1(N2,K2)    ! (20,4)
    INTEGER, KIND :: K2
    INTEGER, LEN  :: N2
    SEQUENCE
    INTEGER(K2)   :: I=1
    PROCEDURE(), POINTER, NOPASS :: ProcPtr => NULL()
  END TYPE


  TYPE(DT(20,4))  :: V1
  TYPE(DT1(20,4)) :: V2

  READ *, V1
  PRINT *,V1

  READ *, V2
  PRINT *,V2


  END


