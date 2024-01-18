!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d356725.f
!*
!*  DATE                       : Sept. 25 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. DEFECT 356725
!234567890123456789012345678901234567890123456789012345678901234567890
program d356725

  implicit none

  integer :: i=1
  print *,null(i)   !<=== diagnose this line
  print *,null(1)   !<=== but doesn't diagnose this line

end program

