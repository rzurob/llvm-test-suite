!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 2 2008
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
!* 3. DEFECT 354585
!234567890123456789012345678901234567890123456789012345678901234567890
module m

   type base(l)
     integer,len  :: l
   end type

end module

program dtParameterInquiryResParam06_d354585
  use m
  implicit none

  call test()
end

subroutine test()
  use m

  integer :: i=4
  character(len=*),parameter :: c="xlf"
  type(base(:)),allocatable :: p1
  allocate(base(i+c%len) :: p1)

end subroutine
