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
!*  Test various forms of the image_set:
!*
!*  THe image set is an array expression
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM sync_images_set_6
  IMPLICIT NONE
  INTEGER :: num_img, me, j
  INTEGER, ALLOCATABLE :: images_set(:)
  INTEGER, SAVE :: work[*]
  INTEGER :: status=0
  CHARACTER(30) :: errstr=" "


  me = this_image()
  num_img = num_images()
  IF (num_img .EQ. 1) THEN
    SYNC IMAGES([1])
    SYNC IMAGES([THIS_IMAGE()])
    SYNC IMAGES([num_images()])
    STOP
  END IF

  ALLOCATE(images_set(num_images()-1))
  images_set = [(j,j=1, me-1),(j,j=me+1,num_img)]

  DO j=1, 100
    CALL sub()
  END DO

  CONTAINS

  SUBROUTINE sub()

    work = me * j
    SYNC IMAGES([(j,j=1, me-1),(j,j=me+1,num_img)], ERRMSG=errstr, stat=status)

    IF (me .NE. NUM_IMAGES()) THEN
      IF ( work[me+1] .NE. (me+1)*j ) then
          ERROR STOP "err 12"
      end if
    ELSE
      IF ( work[1] .NE. j ) THEN
         ERROR STOP "err 13"
      end if
    END IF

    SYNC IMAGES([(j,j=me+1,num_img),(j,j=1, me-1)], ERRMSG=errstr, stat=status)

    IF ( status .NE. 0 ) ERROR STOP "err 16"
    IF ( errstr .NE. " " ) ERROR STOP "err 17"

  END SUBROUTINE

  END