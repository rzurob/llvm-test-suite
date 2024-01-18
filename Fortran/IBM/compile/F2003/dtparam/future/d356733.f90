!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d356733.f
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
!* 1. DEFECT 356733
!234567890123456789012345678901234567890123456789012345678901234567890
program d356733

  implicit none

  integer,pointer :: i
  Data i /null(int(12))/  !<=== doesn't diagnose this line
  i=>null(int(12))        !<=== diagnose this line

end program

