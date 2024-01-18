!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : sync_images_syntax_2.f
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
!*  Test basic syntax : with stat 
!*
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM sync_images_syntax_2
  INTEGER :: num_images
  INTEGER :: status=0
  CHARACTER(30) :: errstr=" "
  INTEGER, SAVE :: me
  INTEGER, SAVE :: work[*]
  

  me = this_image()
  work = me
  SYNC IMAGES(me, stat=status)
  IF ( work[me] .NE. this_image() ) ERROR STOP "err 11" 

  SYNC IMAGES(*)

  work = -this_image()
  SYNC IMAGES([1, (i, i=2, NUM_IMAGES())], ERRMSG=errstr) 
  IF (me .NE. NUM_IMAGES()) THEN
    IF ( work[me+1] .NE. -(me+1) ) THEN 
       print *, me, work[me+1], -(me+1)
       ERROR STOP "err 12" 
    END IF
  ELSE
    IF ( work[1] .NE. -1 ) THEN 
       print *, 1, work[1], -1
       ERROR STOP "err 13" 
    END IF
  END IF

  SYNC IMAGES(*, ERRMSG=errstr, stat=status) 
  work = -work
  SYNC IMAGES(*, ERRMSG=errstr, stat=status) 

  IF (me .NE. NUM_IMAGES()) THEN
    IF ( work[me+1] .NE. (me+1) ) ERROR STOP "err 14" 
  ELSE
    IF ( work[1] .NE. 1 ) ERROR STOP "err 15" 
  END IF

  IF ( status .NE. 0 ) ERROR STOP "err 16"
  IF ( errstr .NE. " " ) ERROR STOP "err 17"

  END
