!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept 08 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3
!* 2. DEFECT 353419
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(l)
      integer,len :: l
      integer(kind(1)+kind(1)) :: i1
      integer(kind(100)+kind(10))   :: i2
   end type
end module

program d353419_1
   use m
   implicit none

   type(base(2)) :: t

   if (kind(t%i1) /= 8) error stop 1
   if (kind(t%i2) /= 8) error stop 2
end program

