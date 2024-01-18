!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : ACHAR
!*
!*  REFERENCE                  : Feature Number 289083
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  -qintsize
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgAchar9
  IMPLICIT NONE

  INTEGER :: I
  INTEGER :: I1
  INTEGER, PARAMETER :: II1(128)=(/(I, I=0,127)/)


  TYPE DT
    INTEGER :: K
    PROCEDURE(), NOPASS, POINTER :: ProcPtr
  END TYPE

  TYPE(DT), PARAMETER :: T=DT(1, NULL())

  CHARACTER :: CC(0:127)

  CC=(/(ACHAR(I=I, KIND=1), I=0, 127)/)

  DO I1 = 0, 127
    IF (ANY((/ACHAR(I1, KIND=IACHAR(ACHAR(I=1, KIND=T%K), KIND=T%K))/)  .NE. CC(I1:I1)) ) ERROR STOP 11
  END DO

  IF (ANY((/ACHAR(I=II1, KIND=IACHAR(ACHAR(I=II1(2), KIND=T%K), KIND=T%K))/)  .NE. CC )) ERROR STOP 21


  END

