!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 11 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3
!* 2. DEFECT 353194
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type :: t(k)
      integer,kind :: k
      integer(k+12) :: val
   end type
end module

program d353194
   use m
   implicit none

   type(t(-8)) :: t1

end

