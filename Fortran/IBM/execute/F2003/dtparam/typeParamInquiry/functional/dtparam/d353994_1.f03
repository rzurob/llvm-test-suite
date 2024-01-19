!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : September 08 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3
!* 2. DEFECT d353994
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k,l)
      integer, kind  :: k
      integer(8),len :: l
      character(l)  :: c1(k:l)
   end type
end module

program d353994_1
  use m
  implicit none
  integer :: i=0
  class(base(k=2,l=:)),allocatable :: b1
  allocate(b1,source=base(k=2,l=7)(c1="xlftest"))
  select type(b1)
    type is(base(2,*))
      print *,b1%l,b1%l%kind,kind(b1%l)
      do i=b1%k,b1%l
      print *,b1%c1(i),(b1%c1(i) /='xlftest')
      end do
  end select

end

