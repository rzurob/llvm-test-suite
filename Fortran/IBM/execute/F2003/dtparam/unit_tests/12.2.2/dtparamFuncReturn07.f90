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
   type base(l)
      integer, len ::  l
      integer :: p
   end type

   contains

   function func (i)
      integer, intent(in) :: i
      type (base(8)), target, save :: tar
      type (base(:)), pointer :: func
      tar%p = 10
      func=>tar
   end function

end module

use m

   type (base(:)), pointer :: ptr
   ptr=>func(10)
   if (ptr%p .ne. 10) stop 1   
end
