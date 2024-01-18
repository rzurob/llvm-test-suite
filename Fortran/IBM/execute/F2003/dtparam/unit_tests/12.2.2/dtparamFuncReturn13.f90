! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : James Ren
!*  DATE                       : 06/07/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : function return with DTP
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
! defect 337711      

module m
type base(baseKind, baseLen1, baseLen2)
   integer, kind :: baseKind   
   integer, len  :: baseLen1, baseLen2
   integer(kind = baseKind) :: baseId(baseLen1 * baseLen2)   
end type

   contains

   function func (i, d)
      integer, intent(in) :: i
      type(base(4, 4, 6)), intent(in) :: d
      type (base(4, :, :)), allocatable :: func
      allocate(func, source = d)
   end function

end module

use m
   type (base(4, :, :)), allocatable :: ptr
   type (base(4, 4, 6)) :: d   
   allocate (ptr, source = func(4,d))
   print *, ptr%basekind    , ptr%baselen1, ptr%baseId(1)%kind
end
