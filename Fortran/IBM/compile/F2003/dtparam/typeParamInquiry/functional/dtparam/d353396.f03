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
!* 2. DEFECT 353396
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(l)
      integer,len :: l
      integer(kind(1)) :: i1
      integer(kind(-1))  :: i2
   end type
end module

program d353396
   use m
   implicit none
   type(base(2)) :: t

   if (t%i1%kind /= kind(1)) error stop 1
   if (t%i2%kind /= kind(1)) error stop 2
end program
