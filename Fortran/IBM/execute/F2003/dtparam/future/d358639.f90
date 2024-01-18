!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d358639.f
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
!*  DEFECT 358639
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(l1)
    integer,len  :: l1
  end type
end module

program d358639

use m
implicit none

type(base(:)),allocatable :: base1(:)
type(base(4))             :: base2=base(4)()

base1=[base(3)(),base(3)(),base(3)()]
base1=base2

print *,base1%l1

end program
