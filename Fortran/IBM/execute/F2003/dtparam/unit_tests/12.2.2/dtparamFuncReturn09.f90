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
type base(baseKind, baseLen1, baseLen2)
   integer, kind :: baseKind
   integer, len  :: baseLen1, baseLen2
   integer(baseKind) :: baseId(baseLen1 : baseLen2)
end type

   contains

   function func (i)
      integer :: j
      integer, intent(in) :: i
      type (base(8, 4, i)), target :: tar
      type (base(8, :, :)), pointer :: func
      tar%baseId = (/(j, j = 1, i-3)/)
      allocate (func, source=tar)
   end function

end module

use m
   integer :: i
   integer :: res(24) = (/(i, i = 1, 24)/)
   type (base(8, :, :)), pointer :: ptr
   ptr=>func(6)
   if (ptr%baseLen1 .ne. 4 .or. ptr%baseLen2 .ne. 6) stop 1
   if (lbound(ptr%baseId, dim=1) .ne. 4) stop 2
   if (ubound(ptr%baseId, dim=1) .ne. 6) stop 3
   if (size(ptr%baseId) .ne. 3) stop 4
   if (kind(ptr%baseId) .ne. 8) stop 5
end
