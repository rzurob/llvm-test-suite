! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_procptr/CrossFeatures1/Null5.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 11, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
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
!*   null()
!*   the defered parameter of the contextual entity
!*  (304623)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM Null5
  IMPLICIT NONE

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: Id
    PROCEDURE(), POINTER, NOPASS :: ProcPtr
  END TYPE

  TYPE (DT(:,4)), POINTER :: V1
  TYPE (DT(:,4)), POINTER :: V2(:)
  TYPE (DT(20,4))          :: V3(2)
  TYPE (DT(20,4))          :: V4(3)

  DATA  V1 /NULL()/,  V2 / NULL() /
  DATA  V3(1) /DT(20,4)(-1, NULL()) /
  DATA  V3(2) /DT(20,4)(-1, NULL()) /
  DATA  V4 /3*DT(20,4)(1, NULL()) /

  IF (ASSOCIATED(V1))  ERROR STOP 11
  IF (ASSOCIATED(V1))  ERROR STOP 13

  IF ( ANY(V3%Id .NE. -1 ) ) ERROR STOP 15
  IF ( ANY(V4%Id .NE.  1 ) ) ERROR STOP 15

  END

