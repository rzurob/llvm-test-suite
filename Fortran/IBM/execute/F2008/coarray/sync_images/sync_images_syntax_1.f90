!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : sync_images_syntax_1.f
!*
!*  DATE                       : July 11 2011
!*
!*  PRIMARY FUNCTIONS TESTED   : SYNC IMAGES
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : CMVC Feature number: 351605.22
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Test basic syntax
!*   (390579)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM sync_images_syntax_1
  INTEGER :: num_images
  INTEGER :: status
  CHARACTER(30) :: errstr
  INTEGER, SAVE :: me
  INTEGER, SAVE :: work[*]


  me = this_image()
  work = me
  SYNC IMAGES(me)
  IF ( work[me] .NE. this_image() ) ERROR STOP "err 11"

  work = -this_image()
  SYNC IMAGES([(i, i=1, NUM_IMAGES())])
  IF (me .NE. NUM_IMAGES()) THEN
    IF ( work[me+1] .NE. -(me+1) ) THEN
      print *, me, work[me+1] , -(me+1)
      ERROR STOP "err 12"
    END IF
  ELSE
    IF ( work[1] .NE. -1 ) ERROR STOP "err 13"
  END IF

  SYNC IMAGES(*)
  work = -work
  SYNC IMAGES(*)

  IF (me .NE. NUM_IMAGES()) THEN
    IF ( work[me+1] .NE. (me+1) ) THEN
       print *, me, work[me+1] , (me+1)
       ERROR STOP "err 14"
    END IF
  ELSE
    IF ( work[1] .NE. 1 ) ERROR STOP "err 15"
  END IF

  END
