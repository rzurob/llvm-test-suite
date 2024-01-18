!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : sync_images_app_1.f
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
!*  Divide images into 3 teams: the number of images in each team differs in size
!*  and achieve synchronization among the teams.
!*
!*  (391383)
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

  SUBROUTINE sub(j)
  INTEGER :: j

    work = me * j

    IF ( me .EQ. 1) THEN   ! team 1
      SYNC IMAGES(images_set(2:num_img-1))
    ELSE IF (me .EQ. num_img) THEN  !team 2
      SYNC IMAGES(images_set(2:num_img-1))
    ELSE  ! team 3, a bigger team
      SYNC IMAGES([images_set(1), images_set(num_img)])
    END IF

    !SYNC ALL ! works here

    IF ( work[mod(me, num_img)+1] .NE. (mod(me, num_img)+1)*j ) then
      print*, j, "=error=",mod(me, num_img)+1,  work[mod(me, num_img)+1], (mod(me, num_img)+1)*j
      ERROR STOP "err 12"
    END IF

    IF ( me .EQ. 1) THEN   ! team 1
      SYNC IMAGES(images_set(2:num_img-1))
    ELSE IF (me .EQ. num_img) THEN  ! team 2
      SYNC IMAGES(images_set(2:num_img-1))
    ELSE  ! team 3
      SYNC IMAGES([images_set(1), images_set(num_img)])
    END IF

  END SUBROUTINE
  END MODULE

  PROGRAM sync_images_app_1
  USE M
  IMPLICIT NONE

  me = this_image()
  num_img = num_images()
  IF (num_img .EQ. 1) THEN
    SYNC IMAGES(1)
    STOP
  ELSE if (num_img .EQ. 2) THEN
    SYNC IMAGES(mod(me, num_img)+1)
    STOP
  END IF

  !If num_img > 2 then do the following
  ALLOCATE(images_set(num_images()))
  images_set = [(j, j=1, num_img)]

  DO j=1, 100
    CALL sub(j)
  END DO


  END
