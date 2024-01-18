!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : INDEX
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


  PROGRAM kindArgIndex9
  IMPLICIT NONE

  INTEGER :: I
  INTEGER :: I1, II(128)=(/(I,I=0,127)/)

  CHARACTER :: CC(128)

  TYPE DT(K)
    INTEGER, KIND :: K=0
!   PROCEDURE(), NOPASS, POINTER :: ProcPtr  ! it is causing ICE now
    LOGICAL(K)    :: L(128) = .TRUE.
  END TYPE

  !TYPE(DT(1)), PARAMETER :: T=DT(1)(NULL())
  TYPE(DT(1)), PARAMETER :: T=DT(1)()

  CC = (/(CHAR(I=I, KIND=1), I=0, 127)/)

  DO I = 1, 128
    IF (     INDEX(STRING=CC(I),SUBSTRING=CC(I), BACK=.NOT.T%L(I), KIND=T%K%KIND)    .NE. 1 )        STOP 11
    IF (KIND(INDEX(STRING=CC(I),SUBSTRING=CC(I), BACK=.NOT.T%L(I), KIND=T%K%KIND))   .NE. T%K%KIND ) STOP 12
    IF (     INDEX(STRING=CC(I),SUBSTRING=CC(I), BACK=     T%L(I), KIND=T%K%KIND)    .NE. 1 )        STOP 13
    IF (KIND(INDEX(STRING=CC(I),SUBSTRING=CC(I), BACK=     T%L(I), KIND=T%K%KIND))   .NE. T%K%KIND ) STOP 14
  END DO

  IF (ANY( INDEX(STRING=CC,SUBSTRING=CC, BACK=.NOT.T%L, KIND=T%K%KIND)    .NE. 1 ))       STOP 21
  IF (KIND(INDEX(STRING=CC,SUBSTRING=CC, BACK=.NOT.T%L, KIND=T%K%KIND))   .NE. T%K%KIND ) STOP 22
  IF (ANY( INDEX(STRING=CC,SUBSTRING=CC, BACK=     T%L, KIND=T%K%KIND)    .NE. 1 ))       STOP 23
  IF (KIND(INDEX(STRING=CC,SUBSTRING=CC, BACK=     T%L, KIND=T%K%KIND))   .NE. T%K%KIND ) STOP 24



  END

