!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : sync_images_stat.f
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
!*  Test the STAT= specifier : its value when the termination condition occurs
!*   
!*  F08:
!*  If the STAT= specifier appears in a SYNC ALL or SYNC IMAGES statement and execution of one of these
!*  statements involves synchronization with an image that has initiated termination, the variable becomes defined
!*  with the value of the constant STAT_STOPPED_IMAGE (13.8.2.24) in the intrinsic module ISO_FORTRAN_ENV
!*  (13.8.2), and the effect of executing the statement is otherwise the same as that of executing the SYNC
!*  MEMORY statement. 
!*
!*  (391408)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  USE ISO_FORTRAN_ENV
  IMPLICIT NONE
  INTEGER :: num_img, me, j
  INTEGER, ALLOCATABLE :: images_set(:) 
  INTEGER, SAVE :: work[*]
  INTEGER :: status=0
  CHARACTER(30) :: errstr=" "

  CONTAINS

  SUBROUTINE sub()

    work[mod(me, num_img)+1] = me  !do something

    IF ( me .EQ. 1) THEN 
      STOP "I want to stop"
    ELSE
      SYNC IMAGES(1, ERRMSG=errstr, stat=status)
      IF (status .NE. STAT_STOPPED_IMAGE) THEN
        print *, me, status, errstr
        ERROR STOP "status is wrong!" 
      END IF
    END IF

  END SUBROUTINE 
  END MODULE
 
  PROGRAM sync_images_stat
  USE M
  IMPLICIT NONE

  me = this_image()
  num_img = num_images()

  IF (num_img .EQ. 1) THEN 
    SYNC IMAGES(1, stat=status)
    IF (status .NE. 0) THEN 
       ERROR STOP "err 11"
    ELSE
      STOP
    END IF 
  END IF


  work = 0
  SYNC IMAGES(*)
  CALL sub()

  END
