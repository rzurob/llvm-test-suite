!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgIchar9
!*
!*  DATE                       : Jun. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : ICHAR
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


  PROGRAM kindArgIchar9
  IMPLICIT NONE

  INTEGER :: I
  INTEGER :: I1, II(128)=(/(I,I=0,127)/)

  !CHARACTER :: CC(128)=(/(CHAR(I=I, KIND=1), I=0, 127)/)
  CHARACTER :: CC(0:127)

!  this is for release 12
! TYPE DT(K)
!   INTEGER, KIND :: K=0
!   PROCEDURE(), NOPASS, POINTER :: ProcPtr
! END TYPE

  TYPE DT
    INTEGER :: K=0
    PROCEDURE(), NOPASS, POINTER :: ProcPtr
  END TYPE

  TYPE(DT), PARAMETER :: T=DT(ProcPtr=NULL())


  CC = (/(CHAR(I=I, KIND=1), I=0, 127)/)

  DO I = 0, 127
    IF (     ICHAR(C=CC(I), KIND=T%K%KIND)    .NE. I )        STOP 11
    IF (KIND(ICHAR(C=CC(I), KIND=T%K%KIND))   .NE. T%K%KIND ) STOP 12
  END DO

  IF (ANY (ICHAR(C=CC, KIND=T%K%KIND)      .NE. II ) )       STOP 21
  IF (KIND(ICHAR(C=CC, KIND=T%K%KIND))     .NE. T%K%KIND )   STOP 22


  END

