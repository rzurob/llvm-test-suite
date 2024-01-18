!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgIShape3
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 29, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : ANY(SHAPE 
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
!*  Returns the shape of an array or a scalar. 
!*    
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgIShape3
  IMPLICIT NONE
  
  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4, I
  INTEGER(8) :: I8
     
  CHARACTER(:), ALLOCATABLE    :: CC(:,:,:,:,:,:,:,:,:) 
  !CHARACTER(128)    :: CC(1:1,2:2,3:3,4:4,5:5,6:6,7:7,8:8,9:9) 
  INTEGER           :: II(9) = 1 
  INTEGER           :: II1(9) = (/0,0,1,1,1,1,1,1,1/) 

  ALLOCATE(CHARACTER(128) :: CC(1:1,2:2,3:3,4:4,5:5,6:6,7:7,8:8,9:9))

  IF (ANY(SHAPE(SOURCE=CC, KIND=1_1 ) .NE. II)) STOP 11
  IF (ANY(SHAPE(SOURCE=CC, KIND=2_2 ) .NE. II)) STOP 12
  IF (ANY(SHAPE(SOURCE=CC, KIND=4_4 ) .NE. II)) STOP 13
  IF (ANY(SHAPE(SOURCE=CC, KIND=8_8 ) .NE. II)) STOP 14

  DEALLOCATE(CC)
  ALLOCATE(CHARACTER(128) :: CC(1:0,2:1,3:3,4:4,5:5,6:6,7:7,8:8,9:9))
  IF (ANY(SHAPE(SOURCE=CC, KIND=1_1 ) .NE. II1)) STOP 21
  IF (ANY(SHAPE(SOURCE=CC, KIND=2_2 ) .NE. II1)) STOP 22
  IF (ANY(SHAPE(SOURCE=CC, KIND=4_4 ) .NE. II1)) STOP 23
  IF (ANY(SHAPE(SOURCE=CC, KIND=8_8 ) .NE. II1)) STOP 24
  DEALLOCATE(CC)

  IF (SIZE(SHAPE(SOURCE=I8, KIND=1_1 )) .NE. 0 ) STOP 31
  IF (SIZE(SHAPE(SOURCE=I4, KIND=2_2 )) .NE. 0 ) STOP 32
  IF (SIZE(SHAPE(SOURCE=I2, KIND=4_4 )) .NE. 0 ) STOP 33
  IF (SIZE(SHAPE(SOURCE=I1, KIND=8_8 )) .NE. 0 ) STOP 34

  END

