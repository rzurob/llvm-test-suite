!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 22 2008
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
!* 2. DEFECT 355327
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type ::base(l)
      integer(8),len  :: l
   end type

end module

program d355327

  use m
  implicit none
  type(base(5)) :: b1
  type(base(:)),allocatable :: b2
  type(base(:)),allocatable :: b3
  type(base(:)),allocatable :: b4

  allocate(base(5) :: b2)
  b3=b1
  b4=b2
  print *,b1%l,b2%l,b3%l,b4%l

end
