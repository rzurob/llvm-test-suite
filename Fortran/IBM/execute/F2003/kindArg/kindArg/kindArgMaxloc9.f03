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


  PROGRAM kindArgMaxloc9
  IMPLICIT NONE



  INTEGER  :: I
  INTEGER  :: II(2,2,2,2,2,2,2,2,2,2)  = 0


  LOGICAL      :: MM (2,2,2,2,2,2,2,2,2,2)=.TRUE.
  LOGICAL      :: MM1(2,2,2,2,2,2,2,2,2,2)=.FALSE.


  II = 0
  II(2,:,:,:,:,:,:,:,:,:) = 1

  IF (ANY( MAXLOC(ARRAY=II )                                 .NE. (/2,1,1,1,1,1,1,1,1,1/)))  ERROR STOP 10
  IF (KIND(MAXLOC(ARRAY=II ))                                .NE. II%KIND)                   ERROR STOP 11

  IF (ANY( MAXLOC(ARRAY=II, KIND=II%KIND )                   .NE. (/2,1,1,1,1,1,1,1,1,1/)))  ERROR STOP 20
  IF (KIND(MAXLOC(ARRAY=II, KIND=II%KIND ))                  .NE. II%KIND)                   ERROR STOP 21

  IF (ANY( MAXLOC(ARRAY=II, MASK=MM, KIND=II%KIND )          .NE. (/2,1,1,1,1,1,1,1,1,1/)))  ERROR STOP 30
  IF (KIND(MAXLOC(ARRAY=II, MASK=MM, KIND=II%KIND ))         .NE. II%KIND)                   ERROR STOP 31

  IF (ANY( MAXLOC(ARRAY=II, MASK=MM, DIM=1, KIND=II%KIND )   .NE. 2))       ERROR STOP 40
  IF (KIND(MAXLOC(ARRAY=II, MASK=MM, DIM=1, KIND=II%KIND ))  .NE. II%KIND)  ERROR STOP 41

  IF (ANY( MAXLOC(ARRAY=II, MASK=MM1, DIM=1, KIND=II%KIND )  .NE. 0))       ERROR STOP 50
  IF (KIND(MAXLOC(ARRAY=II, MASK=MM1, DIM=1, KIND=II%KIND )) .NE. II%KIND)  ERROR STOP 51

  END
