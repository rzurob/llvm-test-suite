!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 22 2008
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
!* 2. TYPE PARAMETER INQUIRY
!* 3. DEFECT 354602
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l)
     integer,len  :: l
   end type
end module

  program dtParameterInquirySelectTypeParam03_d354602
  use m
  implicit none

  type(base(:)),allocatable :: p1
  type(base(:)),pointer :: p2=>null()
  allocate(base(kind(4)) :: p1) ! ICE on this line
  allocate(base(kind(4)) :: p2) ! same ICE on this line

end

