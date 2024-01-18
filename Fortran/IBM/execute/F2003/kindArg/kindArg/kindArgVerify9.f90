!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgVerify9
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jul. 07, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : VERIFY 
!*
!*  REFERENCE                  : Feature Number 289083 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   
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
    IF (     VERIFY(STRING=CC(I),SET=C, BACK=.TRUE., KIND=I%KIND)    .NE. 128 )    STOP 11
    IF (KIND(VERIFY(STRING=CC(I),SET=C, BACK=.TRUE., KIND=I%KIND))   .NE. I%KIND ) STOP 12
    IF (     VERIFY(STRING=CC(I),SET=C, BACK=.FALSE.,KIND=I%KIND)    .NE. 1  )     STOP 13
    IF (KIND(VERIFY(STRING=CC(I),SET=C, BACK=.FALSE.,KIND=I%KIND))   .NE. I%KIND ) STOP 14
  END DO

  IF (ANY( VERIFY(STRING=CC,SET=C, BACK=.TRUE.,  KIND=K%KIND)    .NE. 128 ))       STOP 21
  IF (KIND(VERIFY(STRING=CC,SET=C, BACK=.TRUE.,  KIND=K%KIND))   .NE. K%KIND )     STOP 22
  IF (ANY( VERIFY(STRING=CC,SET=C, BACK=.FALSE., KIND=K%KIND)    .NE. 1 ))         STOP 23
  IF (KIND(VERIFY(STRING=CC,SET=C, BACK=.FALSE., KIND=K%KIND))   .NE. K%KIND )     STOP 24


  END 

