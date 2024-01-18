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
!*  Sync in a fashion of one by one: 1<->2<->, ..., <->n
!*  and mave data between coarrays
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM sync_images_app_10
  IMPLICIT NONE
  INTEGER :: num_img, me, i, j
  INTEGER :: left, right
  INTEGER, SAVE :: work(1000)[*]

  me = this_image()
  num_img = num_images()

  IF (num_img .LT. 2) THEN
    SYNC IMAGES(*)
    STOP
  END IF

  left = me - 1
  right = MOD(me, num_img) + 1
  SYNC IMAGES(*)

  IF (me .EQ. 1) THEN
     work[me] = -1
     SYNC IMAGES(right)
  ELSE
     SYNC IMAGES(left)
      IF ( ANY(work(:)[left] .NE. -left)) ERROR STOP "err 12"
      work(:)[me] = work(:)[left] - 1
      !work[me] = work[left] - 1
     IF ( me .NE. num_img) THEN
       SYNC IMAGES(right)
     END IF
  END IF

  SYNC IMAGES(*)

  END
