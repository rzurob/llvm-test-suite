!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 30, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : SIZE
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


  PROGRAM kindArgSize9

  INTEGER, ALLOCATABLE :: I(:,:)

  INTEGER, PARAMETER   :: II(2)=(/129,129/)


  ALLOCATE(I(-127:1,1:129))

  IF (     SIZE(ARRAY=I, KIND=II%KIND )  .NE. 129**2 )     ERROR STOP 11
  IF (KIND(SIZE(ARRAY=I, KIND=II%KIND )) .NE. II%KIND )    ERROR STOP 12

  IF (     SIZE(ARRAY=I, KIND=II%KIND, DIM=1 )  .NE. 129 )        ERROR STOP 21
  IF (KIND(SIZE(ARRAY=I, KIND=II%KIND, DIM=1 )) .NE. II%KIND )    ERROR STOP 22

  IF (     SIZE(ARRAY=I, KIND=II%KIND, DIM=2 )  .NE. 129 )        ERROR STOP 31
  IF (KIND(SIZE(ARRAY=I, KIND=II%KIND, DIM=2 )) .NE. II%KIND )    ERROR STOP 32

  END

