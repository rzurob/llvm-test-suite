!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : sync_images_set_5.f
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
!*  Test various forms of the image_set:
!*
!*  Image set is a single vale -- sync in a ring fashion
!*  Pass a number through a ring of images and back to its origin.
!*  (390688)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM sync_images_set_5
  IMPLICIT NONE
  INTEGER :: num_img, me, j
  INTEGER, SAVE :: work[*]
  INTEGER :: status=0
  CHARACTER(30) :: errstr=" "


  me = this_image()
  num_img = num_images()

  DO j=1, 10
    work = 0
    SYNC ALL
    CALL int_sub(me, j)
  END DO

  CONTAINS

  SUBROUTINE int_sub(me, num)
  INTEGER :: me, num
    IF (me .EQ. 1) THEN
      IF (me .NE. num_img) THEN
        work[me+1]=num
        SYNC IMAGES(me+1, ERRMSG=errstr, stat=status)
        SYNC IMAGES(num_img, ERRMSG=errstr, stat=status)
        IF (work[me] .NE. num) ERROR STOP 'err 11'
      ELSE
        work[me] = num
        SYNC IMAGES(me)
        IF (work[me] .NE. num) ERROR STOP 'err 12'
      END IF
    ELSE
        SYNC IMAGES(me-1)
        work[MOD(me, num_img)+1]=work[me]
        SYNC IMAGES(MOD(me, num_img)+1)
    END IF

    IF ( status .NE. 0 ) ERROR STOP "err 16"
    IF ( errstr .NE. " " ) ERROR STOP "err 17"
  END SUBROUTINE

  END
