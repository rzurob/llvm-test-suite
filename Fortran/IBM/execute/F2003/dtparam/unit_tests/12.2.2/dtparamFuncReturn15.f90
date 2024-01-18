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
end type

type, extends(base) ::  child(baseLen3)
   integer, len  :: baseLen3
   integer(kind = baseKind) :: childId(baseLen1 + baseLen2 + baseLen3)   
end type

   contains

   function func (d)
      type (child(4, 4, 6, 5)), intent(in) ::d
      type (child(4, :, :, :)), allocatable :: func
      allocate(func, source=d)
   end function

end module

use m
   integer :: k
   type (child(4, 4, 6, 5)) :: d,e   
   d%childId = (/(k, k = 1, 15)/)
   e = d
   print *, e%basekind, e%baselen1, e%baseLen2, e%baseLen3, e%childId
      
end
