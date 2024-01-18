!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d353419_2.f
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
      integer(kind(1)+kind(0)) :: i1
      integer(kind(-1)+kind(100))   :: i2
   end type
end module

program d353419_2
   use m
   implicit none

   type(base(2)) :: t

   print *,t%l
   print *,t%l%kind,kind(t%l)
   print *,t%i1%kind,kind(t%i1)
   print *,t%i2%kind,kind(t%i2)

end

