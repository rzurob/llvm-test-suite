!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryDefferredTypeParam02.f
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
!* 4. DUMMY ARGUMENT IS INTENT(OUT) ALLOCATABLE OR POINTER DERIVED TYPE
!234567890123456789012345678901234567890123456789012345678901234567890
module m

   type ::base(k,l)
      integer(2),kind :: k
      integer(8),len  :: l
   end type

   type(base(2,5)),parameter  :: b5=base(2,5)()
   type(base(2,5)),target     :: b6=base(2,5)()

   contains

     subroutine sub1(b)
        type(base(2,:)),allocatable,intent(out) :: b
        b=b5
     end subroutine
     subroutine sub2(b)
        type(base(2,:)),pointer,intent(out) :: b
        b=>b6
     end subroutine
end module

  program dtParameterInquiryDeferredTypeParam02
  use m
  implicit none

  type(base(2,:)),allocatable :: b1
  type(base(2,:)),pointer  :: b2=>null()
  class(base(2,:)),allocatable :: b3
  class(base(2,:)),pointer :: b4=>null()

  allocate(base(2,10) :: b1)
  call sub1(b1)
  if(b1%k /= 2)                                              error stop 10_4
  if(b1%l /= 5)                                              error stop 11_4
  if(b1%k%kind /=kind(b1%k) .or. b1%k%kind /= 2)             error stop 12_4
  if(b1%l%kind /=kind(b1%l) .or. b1%l%kind /= 8)             error stop 13_4

  call sub2(b2)
  if(b2%k /= 2)                                              error stop 14_4
  if(b2%l /= 5)                                              error stop 15_4
  if(b2%k%kind /=kind(b2%k) .or. b2%k%kind /= 2)             error stop 16_4
  if(b2%l%kind /=kind(b2%l) .or. b2%l%kind /= 8)             error stop 17_4

  call sub3(b3)
  if(b3%k /= 2)                                              error stop 18_4
  if(b3%l /= 5)                                              error stop 19_4
  if(b3%k%kind /=kind(b3%k) .or. b3%k%kind /= 2)             error stop 20_4
  if(b3%l%kind /=kind(b3%l) .or. b3%l%kind /= 8)             error stop 21_4

  allocate(base(2,20) :: b4)
  call sub4(b4)
  if(b4%k /= 2)                                              error stop 22_4
  if(b4%l /= 5)                                              error stop 23_4
  if(b4%k%kind /=kind(b4%k) .or. b4%k%kind /= 2)             error stop 24_4
  if(b4%l%kind /=kind(b4%l) .or. b4%l%kind /= 8)             error stop 25_4

  contains
      subroutine sub3(b)
        class(base(2,:)),allocatable :: b
        allocate(base(2,5) :: b)
      end subroutine

      subroutine sub4(b)
        class(base(2,:)),pointer :: b
        allocate(base(2,5) :: b)
      end subroutine
end
