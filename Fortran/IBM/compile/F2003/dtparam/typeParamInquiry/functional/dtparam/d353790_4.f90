!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d353790_4.f
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
   type base(k1,l1)
      integer,kind :: k1
      integer(k1%kind),len :: l1
   end type
end module

program d353790_4
   use m
   implicit none

   class(base(2,:)),allocatable  :: a1
   allocate(base(kind(2_2),5) :: a1)
   select type(a1)
      type is( base(kind(2_2),*) )
   end select

end
