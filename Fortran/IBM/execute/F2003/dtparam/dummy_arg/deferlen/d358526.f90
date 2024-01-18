!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d358526.f
!*
!*  DATE                       : Nov. 10 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  DEFECT 358526
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp(l1,l2)
    integer,len  :: l1=3
    integer,len  :: l2=4
  end type
end module

program d358526
  use m
  implicit none

  contains

  subroutine sub1(arg)
    type(dtp(*,:)),pointer :: arg
  end subroutine

end program
