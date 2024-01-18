!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : sync_images_syntax_2.f
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
!*  Test basic syntax : diagnostic on syntax
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM sync_images_syntax_2
  INTEGER :: num_images
  INTEGER :: status=0
  CHARACTER(30) :: errstr=" "
  INTEGER, SAVE :: images(1,1) = 1
  INTEGER, SAVE :: work[*]


  SYNC IMAGES()
  SYNC IMAGES(1.)
  SYNC IMAGES(['1'])
  SYNC IMAGES(images)
  SYNC IMAGES(0)
  SYNC IMAGES([0])
  SYNC IMAGES(1, status)
  SYNC IMAGES(1, errstr)
  SYNC IMAGES(*, errmsg=errstr, status)

  ! The following violates rules. However FE does not check for them
  SYNC IMAGES([1,1])
  SYNC IMAGES(num_images()+1)

  END
