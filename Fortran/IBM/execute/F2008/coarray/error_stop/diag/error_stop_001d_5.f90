!*
!*  ===================================================================
!*
!*  TYPE                       : Duagnostic test
!*  FEATURE                    : #351605.31 CAF - ERROR STOP statement
!*
!*  DATE                       : 28 August 2010
!*
!*  REQUIRED COMPILER OPTIONS  :
!*  DEPENDENCIES               :
!*
!*  DESCRIPTION                : Test that the error message from the
!*                               ERROR STOP statement will be printed out
!*                               on stderr and prefixed with "ERROR STOP".
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program error_stop_values
  implicit none
  integer :: selfImage, numImages

  selfImage = this_image()
  numImages = num_images()

  print *, " Before error_stop statement"
    call sleep_(mod(selfImage,(numImages+4))*2+5)
    error stop 126
  sync all

  print *, " After error_stop statement"
  sync all
end program error_stop_values
