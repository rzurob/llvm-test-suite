!#######################################################################
!*
!*  ===================================================================
!*
!*  TYPE                       : Functional test
!*  FEATURE                    : #351605.31 CAF - ERROR STOP statement
!*
!*  DATE                       : 28 August 2010
!*
!*  REQUIRED COMPILER OPTIONS  : -qcaf
!*  DEPENDENCIES               :
!*
!*  DESCRIPTION                : Test that Due to the limitation of POE,
!*                               whatever the stop-code is, the value 1
!*                               is always supplied as the process exit
!*                               status for the ERROR STOP statement
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program error_stop_values
  implicit none
  integer :: selfImage, numImages

  selfImage = this_image()
  numImages = num_images()
  print *, "[",selfImage,"]"," Error Code Value set to: ", TC_ES_VALUE

        ! no need to repeat for many images to issue error stop at the same time
    call sleep_(mod(selfImage,(numImages+2))*3+2)
    error stop TC_ES_VALUE
  sync all

  print *, " FAILURE if this message output!"
  sync all
end program error_stop_values
