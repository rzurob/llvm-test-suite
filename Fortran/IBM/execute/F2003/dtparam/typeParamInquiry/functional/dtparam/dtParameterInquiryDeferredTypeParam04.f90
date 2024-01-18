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
!* 3. WITHOUT COMPONENT
!* 4. USE POINTER ASSIGNMENT AND ALLOCATE
!* 5. FUNCTION RESULT IS DERIVED TYPE
!* 6. DEFECT 354117
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type ::base(k,l)
      integer(2),kind :: k
      integer(8),len  :: l
   end type

   contains

     function fun1()
        type(base(2,:)),allocatable :: fun1
        allocate(base(2,5) :: fun1)
     end function
     function fun2(l)
        type(base(2,:)),pointer :: fun2
        integer(8) :: l
        allocate(base(2,l) :: fun2)
     end function
end module

  program dtParameterInquiryDeferredTypeParam04
  use m
  implicit none

  type(base(2,:)),allocatable :: b1
  type(base(2,:)),pointer  :: b2=>null()
  type(base(2,:)),allocatable :: b3
  integer :: a(5)
  character(5) :: c(5)

  allocate(b1,source=fun1())
  if(b1%k /= 2)                                            error stop 10_4
  if(b1%l /= 5)                                            error stop 11_4
  if(b1%k%kind /=kind(b1%k) .or. b1%k%kind /= 2)           error stop 12_4
  if(b1%l%kind /=kind(b1%l) .or. b1%l%kind /= 8)           error stop 13_4

  associate (x=>fun2(b1%l))
    if(x%k /= 2)                                           error stop 14_4
    if(x%l /= 5)                                           error stop 15_4
    if(x%k%kind /=kind(x%k) .or. x%k%kind /= 2)            error stop 16_4
    if(x%l%kind /=kind(x%l) .or. x%l%kind /= 8)            error stop 17_4
  end associate

   allocate(base(2,5):: b3)
   b3=fun3(b1)
   if(b3%k /= 2)                                           error stop 18_4
   if(b3%l /= 5)                                           error stop 19_4
   if(b3%k%kind /=kind(b3%k) .or. b3%k%kind /= 2)          error stop 20_4
   if(b3%l%kind /=kind(b3%l) .or. b3%l%kind /= 8)          error stop 21_4

  contains
      function fun3(b)
        type(base(2,*)),intent(in) :: b
        type(base(b%k,:)),allocatable :: fun3
        allocate(base(b%k,b%l) :: fun3)
      end function

end

