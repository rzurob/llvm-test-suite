! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/kindArg/kindArg/kindArgIachar9.f
! opt variations: -qnol

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgIachar9
!*
!*  DATE                       : Jun. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : IACHAR
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


  PROGRAM kindArgIachar9
  IMPLICIT NONE

  INTEGER :: I
  INTEGER :: I1, II(128)=(/(I,I=0,127)/)

  !CHARACTER :: CC(128)=(/(ACHAR(I=I, KIND=1), I=0, 127)/)
  CHARACTER :: CC(0:127)

  TYPE DT(N1,D1)    ! (20,4)
    INTEGER, KIND :: D1
    INTEGER, LEN  :: N1
    INTEGER(D1)   :: K
    PROCEDURE(), NOPASS, POINTER :: ProcPtr
  END TYPE

  TYPE(DT(20,4)), PARAMETER :: T=DT(20,4)(1, NULL())

  CC = (/(ACHAR(I=I, KIND=1), I=0, 127)/)

  DO I = 0, 127
    IF (     IACHAR(C=CC(I), KIND=T%K%KIND)    .NE. I )        STOP 11
    IF (KIND(IACHAR(C=CC(I), KIND=T%K%KIND))   .NE. T%K%KIND ) STOP 12
  END DO

  IF (ANY (IACHAR(C=CC, KIND=T%K%KIND)      .NE. II ) )       STOP 21
  IF (KIND(IACHAR(C=CC, KIND=T%K%KIND))     .NE. T%K%KIND )   STOP 22


  END

