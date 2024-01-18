!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : sync_images_set_3.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : July 11 2011
!*  ORIGIN                     : Compiler Development IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : SYNC IMAGES 
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : CMVC Feature number: 351605.22 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*
!*  Test various forms of the image_set:  
!*   
!*  On one image use SYNC IMAGES(*) that corresponds to  SYNC IMAGES(set) on other 
!*  images, where the set is all images except the current image. (may array set issue)
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM sync_images_set_3
  IMPLICIT NONE
  INTEGER :: num_img, me, j
  INTEGER, ALLOCATABLE :: images_set(:) 
  INTEGER, SAVE :: work[*]
  INTEGER :: status=0
  CHARACTER(30) :: errstr=" "
 

  me = this_image()
  num_img = num_images()
  IF (num_img .EQ. 1) THEN 
    SYNC IMAGES(*)
    SYNC IMAGES(1)
    SYNC IMAGES(THIS_IMAGE())
    STOP
  END IF

  ALLOCATE(images_set(num_images()-1))
  images_set = [(j,j=1, me-1),(j,j=me+1,num_img)] 

    DO j=1, 100
      work = me * j
      IF ( me .EQ. num_img) THEN 
        SYNC IMAGES(images_set, ERRMSG=errstr, stat=status)
        !SYNC IMAGES(*, ERRMSG=errstr, stat=status)
      ELSE 
        SYNC IMAGES(*, ERRMSG=errstr, stat=status)
        !SYNC IMAGES(images_set, ERRMSG=errstr, stat=status)
      END IF

      IF (me .NE. NUM_IMAGES()) THEN
        IF ( work[me+1] .NE. (me+1)*j ) then
            ERROR STOP "err 12" 
        end if
      ELSE
        IF ( work[1] .NE. j ) THEN 
           ERROR STOP "err 13" 
       end if
      END IF
 
      IF ( me .EQ. 1) THEN 
        SYNC IMAGES(images_set, ERRMSG=errstr, stat=status)
        !SYNC IMAGES(*, ERRMSG=errstr, stat=status)
      ELSE 
        !SYNC IMAGES(images_set, ERRMSG=errstr, stat=status)
        SYNC IMAGES(*, ERRMSG=errstr, stat=status)
      END IF
    END DO
  

  IF ( status .NE. 0 ) ERROR STOP "err 16"
  IF ( errstr .NE. " " ) ERROR STOP "err 17"

  END
