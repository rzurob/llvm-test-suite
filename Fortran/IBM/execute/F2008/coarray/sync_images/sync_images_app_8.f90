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
!*  sync in the fashion of : 1<-2<-...<-n and then 1->2->...->n
!*  The calculation is circulate a value to all images
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM sync_images_app_8
  IMPLICIT NONE
  INTEGER :: num_img, me, i, j
  INTEGER :: left, right
  INTEGER, SAVE :: work[*]

  me = this_image()
  num_img = num_images()

  IF (num_img .LT. 3) THEN
    SYNC IMAGES(*)
    STOP
  END IF

  left = me - 1
  IF (left == 0) left = num_img
  right = mod(me, num_img) + 1

  SYNC IMAGES(*)

  DO i=1, num_img
    IF (me .EQ. 1) THEN
       work[me] = i
    ELSE
       SYNC IMAGES(left)
    END IF

    work[right] = work[me]

    IF (me .LT. num_img) THEN
       SYNC IMAGES(right)
    END IF

    IF (work .NE. i) ERROR STOP "err 11"
    IF (work[right] .NE. i) ERROR STOP "err 12"

    SYNC IMAGES(*)
  END DO

  END
