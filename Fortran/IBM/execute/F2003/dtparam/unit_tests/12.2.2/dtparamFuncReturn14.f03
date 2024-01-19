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
end type

type, extends(base) ::  child(baseLen3)
   integer, len  :: baseLen3
   integer(kind = baseKind) :: childId(baseLen1 + baseLen2 + baseLen3)
end type

   contains

   function func ()
      integer :: k
      type (child(4, 4, 6, 5)) :: func
      func%childId = (/(k, k = 1, 15)/)
   end function

end module

use m
   type (child(4, :, :, :)), allocatable :: ptr
   type (child(4, 4, 6, 5)) :: d
   allocate (ptr, source = func())
   print *, ptr%basekind, ptr%baselen1, ptr%baseLen2, ptr%baseLen3, ptr%childId
end
