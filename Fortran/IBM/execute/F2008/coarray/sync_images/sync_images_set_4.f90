!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : sync_images_set_4.f
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
!*  Image-set contains its own image index, sync with itself
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM sync_images_set_4
  IMPLICIT NONE
  INTEGER :: num_img, me, j
  INTEGER, SAVE :: work[*]
  INTEGER :: status=0
  CHARACTER(30) :: errstr=" "


  me = this_image()
  num_img = num_images()

  DO j=1, 100
    work = me * j
    CALL int_sub(me)
  END DO

  CONTAINS

  SUBROUTINE int_sub(me)
  INTEGER :: me

      SYNC IMAGES([me], ERRMSG=errstr, stat=status)
      IF ( work[me] .NE. me*j ) THEN
         print*, me, work[me], me*j
         ERROR STOP "err 12"
      END IF

      IF ( status .NE. 0 ) ERROR STOP "err 16"
      IF ( errstr .NE. " " ) ERROR STOP "err 17"
  END SUBROUTINE

  END
