! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/28/2007
!*
!*  DESCRIPTION                : miscellaneous (defect 340417)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
  type :: base(n)
    integer, len :: n
    character(n) :: c
  end type
end module

use m
class(*), pointer :: ptr1
class(base(20)), pointer :: ptr2
allocate(base(*)::ptr1)       ! <-- illegal
allocate(base(*)::ptr2)       ! <-- illegal
end