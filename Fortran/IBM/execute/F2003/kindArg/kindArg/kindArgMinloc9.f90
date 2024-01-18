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


  PROGRAM kindArgMinloc9
  IMPLICIT NONE



  INTEGER  :: I
  INTEGER  :: II(2,2,2,2,2,2,2,2,2,2)  = 0


  LOGICAL  :: MM (2,2,2,2,2,2,2,2,2,2)=.TRUE.
  LOGICAL  :: MM1(2,2,2,2,2,2,2,2,2,2)=.FALSE.


  II = 1
  II(2,:,:,:,:,:,:,:,:,:) = 0

  IF (ANY( MINLOC(ARRAY=II )                                 .NE. (/2,1,1,1,1,1,1,1,1,1/)))  STOP 10
  IF (KIND(MINLOC(ARRAY=II ))                                .NE. II%KIND)                   STOP 11

  IF (ANY( MINLOC(ARRAY=II, KIND=II%KIND )                   .NE. (/2,1,1,1,1,1,1,1,1,1/)))  STOP 20
  IF (KIND(MINLOC(ARRAY=II, KIND=II%KIND ))                  .NE. II%KIND)                   STOP 21

  IF (ANY( MINLOC(ARRAY=II, MASK=MM, KIND=II%KIND )          .NE. (/2,1,1,1,1,1,1,1,1,1/)))  STOP 30
  IF (KIND(MINLOC(ARRAY=II, MASK=MM, KIND=II%KIND ))         .NE. II%KIND)                   STOP 31

  IF (ANY( MINLOC(ARRAY=II, MASK=MM, DIM=1, KIND=II%KIND )   .NE. 2))       STOP 40
  IF (KIND(MINLOC(ARRAY=II, MASK=MM, DIM=1, KIND=II%KIND ))  .NE. II%KIND)  STOP 41

  IF (ANY( MINLOC(ARRAY=II, MASK=MM1, DIM=1, KIND=II%KIND )  .NE. 0))       STOP 50
  IF (KIND(MINLOC(ARRAY=II, MASK=MM1, DIM=1, KIND=II%KIND )) .NE. II%KIND)  STOP 51

  END

