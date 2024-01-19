!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 15 2008
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
!* 3. DEFECT 355097
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l)
      integer(4),len :: l
      character(l)  :: c1(2)
   end type
end module

  program dtParameterInquiryAssumedTypeParam05_d355097
  use m
  implicit none

  integer :: i=0
  type(base(l=:)),pointer :: b1

  allocate(b1,source=base(l=7)(c1="xlftest"))
  print *,lbound(b1%c1,1),ubound(b1%c1,1)
  do i=lbound(b1%c1,1),ubound(b1%c1,1)
     print *,"|",b1%c1(i),"|",(b1%c1(i) /="xlftest")
  enddo

end
