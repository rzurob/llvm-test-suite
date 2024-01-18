!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : sync_images_set_8.f
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
!*  Synchronize with adjacent neighbors to achieve the effect of sync all
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  IMPLICIT NONE
  INTEGER :: num_img, me, j
  INTEGER, ALLOCATABLE :: images_set(:)
  INTEGER, SAVE :: work[*]
  INTEGER :: status=0
  CHARACTER(30) :: errstr=" "
  CONTAINS

  SUBROUTINE sub()

    work = me * j
    IF ( me .EQ. 1) THEN
      SYNC IMAGES([num_img, me, 2], ERRMSG=errstr, stat=status)
    ELSE IF (me .EQ. num_img) THEN
      SYNC IMAGES([me-1, me, 1], ERRMSG=errstr, stat=status)
    ELSE
      SYNC IMAGES([me-1, me, me+1], ERRMSG=errstr, stat=status)
    END IF

    IF (me .NE. NUM_IMAGES()) THEN
      IF ( work[me+1] .NE. (me+1)*j ) then
          print*, me+1, work[me+1], (me+1)*j
          ERROR STOP "err 12"
      end if
    ELSE
      IF ( work[1] .NE. j ) THEN
         ERROR STOP "err 13"
      end if
    END IF

    IF ( me .EQ. 1) THEN
      SYNC IMAGES([num_img,2], ERRMSG=errstr, stat=status)
    ELSE IF (me .EQ. num_img) THEN
      SYNC IMAGES([me-1,1], ERRMSG=errstr, stat=status)
    ELSE
      SYNC IMAGES([me-1, me+1], ERRMSG=errstr, stat=status)
    END IF

    IF ( status .NE. 0 ) ERROR STOP "err 16"
    IF ( errstr .NE. " " ) ERROR STOP "err 17"

  END SUBROUTINE
  END MODULE

  PROGRAM sync_images_set_8
  USE M
  IMPLICIT NONE

  me = this_image()
  num_img = num_images()
  IF (num_img .EQ. 1) THEN
    SYNC IMAGES(1)
    STOP
  END IF

  ALLOCATE(images_set(num_images()))

  DO j=1, 100
    CALL sub()
  END DO


  END
