!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : typeParamInquiryDTIntrinsic05.f
!*
!*  DATE                       : July 7 2008
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
!* 2. TYPE PARAMETER INQUIRY FOR INTRINSIC TYPE
!* 3. INQUIRY TYPE PARAMETER OF FUNCTION RESULT
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base
     integer :: i1
     integer(2) :: i2(3)
     complex :: x1
     complex(8) :: x2(3)
     logical :: l1
     logical(1) :: l2(2)
     character(len=3) :: c1
     character(len=2) :: c2(3)

     character(:),allocatable :: c3
     character(:),allocatable :: c4(:)

     character(:),pointer  :: c5
     character(:),pointer  :: c6(:)
   end type
   contains
     function fun(dt)
       type(base) :: dt,fun

       fun=dt
     end function

end module

  program typeParamInquiryDTIntrinsic05
  use m
  implicit none

  type(base) :: b1
  type(base),allocatable :: b2
  type(base),pointer :: b3=>null()
  type(base),target  :: b4
  type(base) :: result

  b3=>b4

  allocate(b1%c3,source='')
  allocate(b1%c4(2:4),source="xlftest"(5:4))
  allocate(b1%c5,source='//')
  allocate(b1%c6(lbound(b1%c4,1):ubound(b1%c4,1)),source=['xyz','abc'])

  result=fun(b1)
  call verifyKind(result)
  call verifyLen1(result)

  allocate(b2)
  allocate(b2%c3,source='123')
  allocate(b2%c4(2:4),source=['****','####','%%%%'])
  allocate(character(-10) ::b2%c5)
  allocate(b2%c6(4:4),source="xlf"//"test")

  result=fun(b2)
  call verifyKind(result)
  call verifyLen2(result)

  b3%c3=b2%c3
  b3%c4=b2%c4
  b3%c5=>b2%c5
  b3%c6=>b2%c6

  result=fun(b3)
  call verifyKind(result)
  call verifyLen2(result)

    contains
      subroutine verifyKind(dt)
        type(base) :: dt
        if(dt%i1%kind /= kind(dt%i1) .or. dt%i1%kind /= 4) error stop 10_4
        if(dt%i2%kind /= kind(dt%i2) .or. dt%i2%kind /= 2) error stop 11_4
        if(dt%x1%kind /= kind(dt%x1) .or. dt%x1%kind /= 4) error stop 12_4
        if(dt%x2%kind /= kind(dt%x2) .or. dt%x2%kind /= 8) error stop 13_4
        if(dt%c1%kind /= kind(dt%c1) .or. dt%c1%kind /= 1) error stop 14_4
        if(dt%c2%kind /= kind(dt%c2) .or. dt%c2%kind /= 1) error stop 15_4
        if(dt%c3%kind /= kind(dt%c3) .or. dt%c3%kind /= 1) error stop 16_4
        if(dt%c4%kind /= kind(dt%c4) .or. dt%c4%kind /= 1) error stop 17_4
        if(dt%c5%kind /= kind(dt%c5) .or. dt%c5%kind /= 1) error stop 18_4
        if(dt%c6%kind /= kind(dt%c6) .or. dt%c6%kind /= 1) error stop 19_4
        end subroutine

       subroutine verifyLen1(dt)
        type(base) :: dt

        if(dt%c1%len  /= len(dt%c1)  .or. dt%c1%len  /= 3) error stop 20_4
        if(dt%c2%len  /= len(dt%c2)  .or. dt%c2%len  /= 2) error stop 21_4
        if(dt%c3%len  /= len(dt%c3)  .or. dt%c3%len  /= 0) error stop 22_4
        if(dt%c4%len  /= len(dt%c4)  .or. dt%c4%len  /= 0) error stop 23_4
        if(dt%c5%len  /= len(dt%c5)  .or. dt%c5%len  /= 2) error stop 24_4
        if(dt%c6%len  /= len(dt%c6)  .or. dt%c6%len  /= 3) error stop 25_4

        end subroutine

       subroutine verifyLen2(dt)
        type(base) :: dt

        if(dt%c1%len  /= len(dt%c1)  .or. dt%c1%len  /= 3) error stop 26_4
        if(dt%c2%len  /= len(dt%c2)  .or. dt%c2%len  /= 2) error stop 27_4
        if(dt%c3%len  /= len(dt%c3)  .or. dt%c3%len  /= 3) error stop 28_4
        if(dt%c4%len  /= len(dt%c4)  .or. dt%c4%len  /= 4) error stop 29_4
        if(dt%c5%len  /= len(dt%c5)  .or. dt%c5%len  /= 0) error stop 30_4
        if(dt%c6%len  /= len(dt%c6)  .or. dt%c6%len  /= 7) error stop 31_4

        end subroutine
end
