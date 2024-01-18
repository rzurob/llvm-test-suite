!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryWithoutComp03_d353615.f
!*
!*  DATE                       : July 10 2008
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
!* 3. DIFFERENT TYPE PARAMETER
!* 4. WITHOUT COMPONENT,DEFAULT INTIALIZATION
!* 5. DEFECT 353615
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(k1,k2,l1,l2)
     integer(1),kind  :: k1=2
     integer(k1),kind :: k2=k1
     integer(k1),len  :: l1=k1
     integer(k1),len  :: l2=k1
   end type
end module
  program dtParameterInquiryWithoutComp03_d353615
  use m
  implicit none

  type(base)  :: t

  if(t%k1 /= 2)                                   error stop 10_4
  if(t%k2 /= 2)                                   error stop 11_4
  if(t%k1%kind /= kind(t%k1) .or. t%k1%kind /=1)  error stop 12_4
  if(t%k2%kind /= kind(t%k2) .or. t%k2%kind /=2)  error stop 13_4
  if(t%l1 /= 2)                                   error stop 14_4
  if(t%l2 /= 2)                                   error stop 15_4
  if(t%l1%kind /= kind(t%l1) .or. t%l1%kind /=2)  error stop 16_4
  if(t%l2%kind /= kind(t%l2) .or. t%l2%kind /=2)  error stop 17_4


  end
