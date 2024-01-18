!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryDeferredTypeParam01.f
!*
!*  DATE                       : July 21 2008
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
!* 3. WITHOUT COMPONENT
!* 4. USE ASSIGNMENT,ALLOCATE
!* 5. DEFECT 355327
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type ::base(k,l)
      integer(2),kind :: k
      integer(8),len  :: l
   end type

end module

  program dtParameterInquiryDeferredTypeParam01
  use m
  implicit none

  type(base(2,5)),target :: b1
  type(base(2,:)),allocatable :: b2
  type(base(2,:)),pointer  :: b3=>null()
  type(base(2,:)),allocatable :: b4
  type(base(2,:)),pointer :: b5=>null()

  allocate(base(2,5) :: b2)
  b3=>b1
  b4=b2
  allocate(b5,source=b1)

  if(b1%k /= 2 .or. b2%k /= 2 .or. b3%k /=2 .or.  &
         b4%k /=2 .or. b5%k /= 2)                            error stop 10_4
  if(b1%l /= 5 .or. b2%l /= 5 .or. b3%l /=5 .or.  &
         b4%l /=5 .or. b5%l /= 5)                            error stop 11_4
  if(b1%k%kind /=kind(b1%k) .or. b1%k%kind /= 2)             error stop 12_4
  if(b1%l%kind /=kind(b1%l) .or. b1%l%kind /= 8)             error stop 13_4

  if(b2%k%kind /=kind(b2%k) .or. b2%k%kind /= 2)             error stop 14_4
  if(b2%l%kind /=kind(b2%l) .or. b2%l%kind /= 8)             error stop 15_4

  if(b3%k%kind /=kind(b3%k) .or. b3%k%kind /= 2)             error stop 16_4
  if(b3%l%kind /=kind(b3%l) .or. b3%l%kind /= 8)             error stop 17_4

  if(b4%k%kind /=kind(b4%k) .or. b4%k%kind /= 2)             error stop 18_4
  if(b4%l%kind /=kind(b4%l) .or. b4%l%kind /= 8)             error stop 19_4

  if(b5%k%kind /=kind(b5%k) .or. b5%k%kind /= 2)             error stop 20_4
  if(b5%l%kind /=kind(b5%l) .or. b5%l%kind /= 8)             error stop 21_4

end
