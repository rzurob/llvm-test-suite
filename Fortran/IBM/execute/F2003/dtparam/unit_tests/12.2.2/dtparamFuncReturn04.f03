! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/07/2007
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : function return with DTP
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type base(l)
      integer, len ::  l
      integer :: p
   end type

   contains

   function func (i)
      integer, intent(in) :: i
      type (base(i)) :: func
      func%p = i
   end function

end module

use m

   type (base(:)), pointer :: ptr
   allocate(ptr, source = func(10))
   if (ptr%p .ne. 10) error stop 1
end
