!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 29, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : SHAPE
!*
!*  REFERENCE                  : Feature Number 289083
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGEK(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  - qintsize
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgShape9

  INTEGER, ALLOCATABLE :: I(:,:)

  INTEGER, PARAMETER      :: II(2)=(/129,129/)


  ALLOCATE(I(-127:1,1:129))

  IF (ANY( SHAPE(SOURCE=I, KIND=II%KIND )  .NE. II ))        ERROR STOP 11
  IF (KIND(SHAPE(SOURCE=I, KIND=II%KIND )) .NE. II%KIND )    ERROR STOP 12

  END
