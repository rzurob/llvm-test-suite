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
!*  SYNC IMAGES(*)  -- ALL images sync with SYNC IMAGES(*)
!*  (390666)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM sync_images_set_2
  IMPLICIT NONE
  INTEGER :: num_img, me, j
  INTEGER, SAVE :: work[*]
  INTEGER :: status=0
  CHARACTER(30) :: errstr=" "


  me = this_image()
  num_img = num_images()

  IF (num_img .EQ. 1) THEN
    work = me
    SYNC IMAGES(*, ERRMSG=errstr, stat=status )
    IF ( work[num_images()] .NE. this_image() ) ERROR STOP "err 11"
  ELSE

    DO j=1, 100
      work = me * j
      IF ( me .EQ. num_img) THEN
        SYNC IMAGES(*, ERRMSG=errstr, stat=status)
      ELSE
        SYNC IMAGES(*, ERRMSG=errstr, stat=status)
      END IF

      IF (me .NE. NUM_IMAGES()) THEN
        IF ( work[me+1] .NE. (me+1)*j ) then
            ERROR STOP "err 12"
        end if
      ELSE
        IF ( work[1] .NE. j ) ERROR STOP "err 13"
      END IF

      IF ( me .EQ. 1) THEN
        SYNC IMAGES(*, ERRMSG=errstr, stat=status)
      ELSE
        SYNC IMAGES(*, ERRMSG=errstr, stat=status)
      END IF
    END DO

  END IF

  IF ( status .NE. 0 ) ERROR STOP "err 16"
  IF ( errstr .NE. " " ) ERROR STOP "err 17"

  END
