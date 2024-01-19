!*********************************************************************
!*  ===================================================================
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
!* 4. DUMMY ARGUMENT IS ALLOCATABLE OR POINTER DERIVED TYPE
!* 5. LENGTH PARAMTER GET FROM ARGUMENT
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type ::base(k,l)
      integer(2),kind :: k
      integer(8),len  :: l
   end type

   contains

     subroutine sub1(b,a)
        type(base(2,:)),allocatable,intent(out) :: b
        integer,intent(in) :: a(:)
        allocate(base(2,size(a,1)) :: b)
     end subroutine
     subroutine sub2(b,l)
        type(base(2,:)),pointer,intent(out) :: b
        integer(8) :: l
        allocate(base(2,l) :: b)
     end subroutine
end module

  program dtParameterInquiryDeferredTypeParam03
  use m
  implicit none

  type(base(2,:)),allocatable :: b1
  type(base(2,:)),pointer  :: b2=>null()
  class(base(2,:)),allocatable :: b3
  class(base(2,:)),pointer :: b4=>null()
  integer :: a(5)
  character(5) :: c(5)

  call sub1(b1,a)
  if(b1%k /= 2)                                              error stop 10_4
  if(b1%l /= 5)                                              error stop 11_4
  if(b1%k%kind /=kind(b1%k) .or. b1%k%kind /= 2)             error stop 12_4
  if(b1%l%kind /=kind(b1%l) .or. b1%l%kind /= 8)             error stop 13_4

  call sub2(b2,b1%l)
  if(b2%k /= 2)                                              error stop 14_4
  if(b2%l /= 5)                                              error stop 15_4
  if(b2%k%kind /=kind(b2%k) .or. b2%k%kind /= 2)             error stop 16_4
  if(b2%l%kind /=kind(b2%l) .or. b2%l%kind /= 8)             error stop 17_4

  call sub3(b3,c)
  if(b3%k /= 2)                                              error stop 18_4
  if(b3%l /= 5)                                              error stop 19_4
  if(b3%k%kind /=kind(b3%k) .or. b3%k%kind /= 2)             error stop 20_4
  if(b3%l%kind /=kind(b3%l) .or. b3%l%kind /= 8)             error stop 21_4

  call sub4(b4,c%len)
  if(b4%k /= 2)                                              error stop 22_4
  if(b4%l /= 5)                                              error stop 23_4
  if(b4%k%kind /=kind(b4%k) .or. b4%k%kind /= 2)             error stop 24_4
  if(b4%l%kind /=kind(b4%l) .or. b4%l%kind /= 8)             error stop 25_4

  contains
      subroutine sub3(b,c)
        class(base(2,:)),allocatable :: b
        character(5),intent(in) :: c(:)
        allocate(base(2,c%len) :: b)
      end subroutine

      subroutine sub4(b,l)
        class(base(2,:)),pointer :: b
        integer(4),intent(in) :: l
        allocate(base(2,l) :: b)
      end subroutine
end
