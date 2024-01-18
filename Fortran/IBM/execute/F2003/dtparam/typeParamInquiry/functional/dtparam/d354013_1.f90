!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d354013_1.f
!*
!*  DATE                       : September 06 2008
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
!* 2. DEFECT d354013_1
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l)
      integer,len   :: l
      character(l)  :: c
   end type
end module

program d354013_1
  use m
  implicit none

  type(base(:)),allocatable :: b
  allocate(b,source=base(5)(c="hello"))
  print *,b%l
  print *,b%c%len,len(b%c),(b%c%len /= 5)
  print *,b%c

end

