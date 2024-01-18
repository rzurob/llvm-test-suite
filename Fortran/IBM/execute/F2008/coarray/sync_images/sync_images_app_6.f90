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
!*  Mixed Use of sync images(index_arr) with sync images(index)
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM sync_images_app_6
  IMPLICIT NONE
  INTEGER :: num_img, me, i, j
  INTEGER, SAVE :: work[*]

  me = this_image()
  num_img = num_images()

  IF (num_img .EQ. 1) STOP

  DO i=1, num_img
    IF (me .EQ. i) THEN
      work = i
      SYNC IMAGES([(j, j=1,i-1),(j, j=i+1,num_img)])
      IF (work .NE. i) ERROR STOP "err 11"
      IF (work[mod(i,num_img)+1] .NE. i) ERROR STOP "err 12"
    ELSE
      CALL intsub(work, i)
      SYNC IMAGES(i)
      IF (work .NE. i) ERROR STOP "err 13"
         !print*, "before if ", me, work[mod(i,num_img)+1], i
      IF (work[mod(i,num_img)+1] .NE. i) THEN
         print*, me, work[mod(i,num_img)+1], i
         ERROR STOP "err 14"
      END IF
    END IF
    SYNC IMAGES(*)
  END DO

  CONTAINS

  SUBROUTINE intsub(work, i)
  INTEGER :: work, i
    work = i
  END SUBROUTINE

  END
