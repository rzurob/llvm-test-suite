!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : sync_images_app_9.f
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
!*  Sync in a fashion of one by one: 2->1, .., n->1 and then 1->2,..., 1->n
!*  (The most slow sync form)
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM sync_images_app_9
  IMPLICIT NONE
  INTEGER :: num_img, me, i, j
  INTEGER :: left, right
  INTEGER, SAVE :: work[*]

  me = this_image()
  num_img = num_images()

  IF (num_img .LT. 2) THEN
    SYNC IMAGES(*)
    STOP
  END IF

  SYNC IMAGES(*)

  DO i=1, num_img
    IF (me .EQ. i) THEN
       work[me] = i
       DO j=1, i-1
         work[j] = work[me]
         SYNC IMAGES(j)
       END DO
       DO j=i+1, num_img
         work[j] = work[me]
         SYNC IMAGES(j)
       END DO
    ELSE
       SYNC IMAGES(i)
       IF (work[me] .NE. work[i]) ERROR STOP "err 11"
       IF (work[me] .NE. i) ERROR STOP "err 12"
    END IF

    SYNC IMAGES(*)
  END DO

  END
