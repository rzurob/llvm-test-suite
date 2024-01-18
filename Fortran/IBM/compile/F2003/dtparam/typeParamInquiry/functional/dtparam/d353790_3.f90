!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d353790_3.f
!*
!*  DATE                       : July 28 2008
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
!* 2. DEFECT 353790
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type ::base(k,l)
      integer(2),kind :: k
      integer(8),len  :: l
   end type

end module

program d353790_3
   use m
   implicit none

   type(base(4,:)),allocatable:: b1
   allocate(base(kind(4_4),len('abc')) :: b1)

end
