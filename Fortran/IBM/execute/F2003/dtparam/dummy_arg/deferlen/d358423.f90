!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d358423.f
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
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp(l1)
     integer,len :: l1
  end type
end module

program d358423

use m
implicit none

type(dtp(3)),pointer :: a1=>null()
allocate(dtp(3) :: a1)
call sub(a1)
contains
  subroutine sub(arg)
    type(dtp(*)),pointer :: arg
    print *,arg%l1
    allocate(dtp(*) :: arg)
  end subroutine

end program

