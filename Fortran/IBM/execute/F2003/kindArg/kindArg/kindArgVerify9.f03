!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jul. 07, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : VERIFY
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


  PROGRAM kindArgVerify9

  INTEGER :: I

  CHARACTER(128) :: CC(0:127)
  CHARACTER(127) :: C

  DO J=1, 127
    C(J:J)=ACHAR(J)
  END DO
  DO I=0, 127
  DO J=1, 128
    CC(I)(J:J)=ACHAR(0)
  END DO
  END DO


  DO I = 0, 127
    IF (     VERIFY(STRING=CC(I),SET=C, BACK=.TRUE., KIND=I%KIND)    .NE. 128 )    ERROR STOP 11
    IF (KIND(VERIFY(STRING=CC(I),SET=C, BACK=.TRUE., KIND=I%KIND))   .NE. I%KIND ) ERROR STOP 12
    IF (     VERIFY(STRING=CC(I),SET=C, BACK=.FALSE.,KIND=I%KIND)    .NE. 1  )     ERROR STOP 13
    IF (KIND(VERIFY(STRING=CC(I),SET=C, BACK=.FALSE.,KIND=I%KIND))   .NE. I%KIND ) ERROR STOP 14
  END DO

  IF (ANY( VERIFY(STRING=CC,SET=C, BACK=.TRUE.,  KIND=K%KIND)    .NE. 128 ))       ERROR STOP 21
  IF (KIND(VERIFY(STRING=CC,SET=C, BACK=.TRUE.,  KIND=K%KIND))   .NE. K%KIND )     ERROR STOP 22
  IF (ANY( VERIFY(STRING=CC,SET=C, BACK=.FALSE., KIND=K%KIND)    .NE. 1 ))         ERROR STOP 23
  IF (KIND(VERIFY(STRING=CC,SET=C, BACK=.FALSE., KIND=K%KIND))   .NE. K%KIND )     ERROR STOP 24


  END
