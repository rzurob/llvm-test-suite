!*********************************************************************
!*  ===================================================================
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
!*  Sync image small application :
!*
!*  Use of sync images(*)
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM sync_images_app_4
  IMPLICIT NONE
  INTEGER :: num_img, me, i, j
  INTEGER, SAVE :: work[*]

  me = this_image()
  num_img = num_images()

  IF (num_img .EQ. 1) STOP

  DO i=1, num_img-1
    IF (me .EQ. num_img) THEN
      CALL intsub(work, i)
      SYNC IMAGES(*)
      IF (work .NE. i) ERROR STOP "err 11"
      IF (work[1] .NE. i) ERROR STOP "err 12"
    ELSE
      CALL intsub(work, i)
      SYNC IMAGES(*)
      IF (work .NE. i) ERROR STOP "err 13"
      IF (work[me+1] .NE. i) ERROR STOP "err 14"
    END IF
    SYNC IMAGES(*)
  END DO

  CONTAINS

  SUBROUTINE intsub(work, i)
  INTEGER :: work, i
    work = i
  END SUBROUTINE

  END