!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryAssumedTypeParam03_d353958.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 18 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3 
!* 2. TYPE PARAMETER INQUIRY
!* 3. DEFECT 353958   
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1,l2)
      integer(2),len  :: l1
      integer(4),len  :: l2
      integer(8) :: i2(l2-l1:l1+l2)
   end type
   contains
   subroutine getbase(b)
      type(base(l1=*,l2=*)),intent(in) :: b
      if(lbound(b%i2,1) /= 3)                   error stop 10_4
      if(ubound(b%i2,1) /= 9)                   error stop 11_4
   end subroutine
end module

  program dtParameterInquiryAssumedTypeParam03_d353958
  use m
  implicit none

  type(base(3,6)) :: t1
  type(base(6,3)) :: t2
  call getbase(t1)
  if(lbound(t2%i2,1) /= -3)                     error stop 12_4
  if(ubound(t2%i2,1) /= 9)                     error stop 13_4

end
