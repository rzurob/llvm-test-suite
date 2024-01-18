!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : sync_images_set_1.f
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
!*  Test various forms of the image_set: array with vector subscript. 
!*   
!*  (390633)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM sync_images_set_1
  INTEGER :: num_img
  INTEGER, ALLOCATABLE :: image_set(:)
  INTEGER, SAVE :: work[*]
  INTEGER :: status=0
  CHARACTER(30) :: errstr=" "
 

  me = this_image()
  num_img = num_images()

  IF (num_img .EQ. 1) THEN 
    work = me
    SYNC IMAGES([1], ERRMSG=errstr, stat=status )
    IF ( work[me] .NE. this_image() ) ERROR STOP "err 11" 
  ELSE
    ALLOCATE(image_set(num_img-1))
    image_set(:) = [(i, i=num_img-1,1,-1)]
  
    DO j=1, 100
      work = me * j
      IF ( me .EQ. 1) THEN 
        SYNC IMAGES(image_set(image_set), ERRMSG=errstr, stat=status)
      ELSE 
        SYNC IMAGES(1, ERRMSG=errstr, stat=status)
      END IF
      IF (me .NE. NUM_IMAGES()) THEN
        IF ( work[me+1] .NE. (m+1)*j ) THEN 
           print*, j, me , work[me+1],  (m+1)*j
           ERROR STOP "err 12" 
        END IF
      ELSE
        IF ( work[1] .NE. j ) ERROR STOP "err 13" 
      END IF
      SYNC IMAGES(*)
    END DO
  END IF

  IF ( status .NE. 0 ) ERROR STOP "err 16"
  IF ( errstr .NE. " " ) ERROR STOP "err 17"

  END
