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
!*  Synchronizes with both of its neighbors in a circular fashion.
!*  Segment order preserved with neighbors.
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM sync_images_app_7
  IMPLICIT NONE
  INTEGER :: num_img, me, i, j
  INTEGER :: left, right
  INTEGER, SAVE :: work[*]

  me = this_image()
  num_img = num_images()

  IF (num_img == 1) THEN
    SYNC IMAGES(1)
    STOP
  ELSE IF (num_img == 2) THEN
    IF ( me == 1) THEN
      SYNC IMAGES(2)
    ELSE
      SYNC IMAGES(1)
    END IF
    STOP
  END IF

  left = me - 1
  IF (left == 0) left = num_img
  right = mod(me, num_img) + 1

  DO i=1, num_img
      CALL intsub(work, i)
      SYNC IMAGES([left, right])
      IF (work .NE. i) ERROR STOP "err 11"
      IF (work[left]  .NE. i) ERROR STOP "err 12"
      IF (work[right] .NE. i) ERROR STOP "err 13"
      SYNC IMAGES([right, left])
  END DO

  CONTAINS

  SUBROUTINE intsub(work, i)
  INTEGER :: work, i
    work = i
  END SUBROUTINE

  END
