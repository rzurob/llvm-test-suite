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
! defect 337716

module m
type base(baseLen1, baseLen2)
   integer, len  :: baseLen1, baseLen2
   integer :: baseId(baseLen1 * baseLen2)
end type

   contains

   function func (i, j)
      integer, intent(in) :: i, j
      type (base(:, :)), pointer :: func
      type (base(i, j)), target :: tar
      tar%baseId = (/(k, k = 1, i*j)/)
      allocate (func, source=tar)
   end function

end module

use m
   type (base(:, :)), allocatable :: ptr
   allocate (ptr, source = func(4,6))
   print *, ptr%baseId
end