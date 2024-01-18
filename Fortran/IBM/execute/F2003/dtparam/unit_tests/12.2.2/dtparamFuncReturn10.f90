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
   type (base(:, :)), pointer :: ptr
   integer, pointer :: p(:)    
   integer :: arr(10)   
   ptr=>func(4,6)
   p(4:)=>ptr%baseId
   print *, ptr   
end
