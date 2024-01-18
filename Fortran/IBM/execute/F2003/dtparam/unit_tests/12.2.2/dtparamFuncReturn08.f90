! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/07/2007
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
   integer(baseKind) :: baseId(baseLen1 * baseLen2)
end type

      type (base(8, 4, 6)), target :: tar
   contains

   function func (i)
      integer :: j
      integer, intent(in) :: i
      type (base(8, :, :)), pointer :: func
      tar%baseId = (/(j, j = 1, 24)/)
      func=>tar
   end function

end module

use m
   integer :: i
   integer :: res(24) = (/(i, i = 1, 24)/)
   type (base(8, :, :)), pointer :: ptr
   ptr=>func(6)
   if (.not. all(ptr%baseId .eq. res)) stop 1
end
