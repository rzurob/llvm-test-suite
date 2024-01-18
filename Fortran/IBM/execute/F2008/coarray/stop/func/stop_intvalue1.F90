!#######################################################################
!*
!*  ===================================================================
!*
!*  TYPE                       : Functional test
!*  FEATURE                    : #351605.31 CAF - STOP statement
!*
!*  DATE                       : 19 Oct 2010
!*
!*  REQUIRED COMPILER OPTIONS  : -qcaf
!*  DEPENDENCIES               :
!*
!*  DESCRIPTION                : Test that Due to the limitation of POE,
!*                               whatever the stop-code is, the value 0
!*                               is always supplied as the process exit
!*                               status for the STOP statement
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program stop_values

  print *, "[",this_image(),"]"," Error Code Value set to: ", TC_STOP_VALUE
  stop TC_STOP_VALUE
  sync all

  print *, " FAILURE if this message output!"
  sync all

end program stop_values
