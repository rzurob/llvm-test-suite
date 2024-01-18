!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryWithoutComp01_d353191.f
!*
!*  DATE                       : July 23 2008
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
!* 3. PASS NEGATIVE PARAMETER
!* 4. DEFECT 353191
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type :: t(k,l)
    integer,kind :: k
    integer,len  :: l
  end type
end module

  program dtParameterInquiryWithoutComp01_d353191
  use m
  implicit none

  type(t(-2,-2)) :: t1
  print *,t1%k,t1%l

end
