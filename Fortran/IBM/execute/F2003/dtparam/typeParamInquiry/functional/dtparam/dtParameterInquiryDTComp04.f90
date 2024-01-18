!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryDTComp04.f
!*
!*  DATE                       : August 21 2008
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
!* 3. INQUIRY TYPE PARAMETER OF DERIVED TYPE COMPONENT
!* 4. DERIVED TYPE COMPONENT IS POLYMORPHIC
!* 5. DEFECT 355440
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type :: A(l1)
     integer,len  :: l1
     character(:),pointer :: ch1=>null()
   end type

   type,extends(A) :: B(l2)
     integer,len    :: l2
     character(:),pointer  :: ch2=>null()
   end type

   type :: C(l3)
      integer,len     :: l3
      character(3)    :: ch3
      CLASS(*),pointer      :: a1=>null()
      CLASS(A(l3)),pointer  :: a2=>null()
   end type

end module

program dtParameterInquiryDTComp04
  use m
  implicit none

  class(A(:)),pointer :: t1
  class(A(:)),pointer :: t2
  character(len=5),target :: cha1="hello"
  character(len=7),target :: cha2="xlftest"

  type(C(:)),allocatable :: c1

  allocate(c1,source=C(3)(ch3="xlf"))

  allocate(t1,source=A(cha1%len)(cha1))
  allocate(t2,source=A(3)(cha1(1:3)))

  c1%a1=>t1
  c1%a2=>t2

  call check(c1)

  c1%a1=>null()
  c1%a2=>null()

  deallocate(t1,t2)

  allocate(t1,source=B(cha1%len,cha2%len)(cha1,cha2))
  allocate(t2,source=B(3,3)(cha1(1:3),cha2(5:)))

  c1%a1=>t1
  c1%a2=>t2

  call check(c1)

  contains

    subroutine check(dt)
       type(C(*)) :: dt
       if(dt%l3 /= 3)                                       error stop 10_4
       if(dt%ch3%len /= len(dt%ch3) .or. dt%ch3%len /= 3)   error stop 11_4
       if(dt%ch3 /= "xlf")                                  error stop 12_4

       select type(x=>dt%a1)
          type is(A(*))
            if(x%l1 /= 5)                                   error stop 13_4
            if(x%ch1%len /= len(x%ch1) .or. x%ch1%len /=5)  error stop 14_4
            if(x%ch1 /= "hello")                            error stop 15_4

          type is(B(*,*))
            if(x%l1 /= 5)                                   error stop 13_4
            if(x%ch1%len /= len(x%ch1) .or. x%ch1%len /=5)  error stop 14_4
            if(x%ch1 /= "hello")                            error stop 15_4
            if(x%l2 /= 7)                                   error stop 16_4
            if(x%ch2%len /= len(x%ch2) .or. x%ch2%len /=7)  error stop 17_4
            if(x%ch2 /= "xlftest")                          error stop 18_4

          class default
            error stop 101_4
       end select

       select type(x=>dt%a2)
          type is(A(*))
             if(x%l1 /= 3)                                  error stop 19_4
             if(x%ch1%len /= len(x%ch1) .or. x%ch1%len /=3) error stop 20_4
             if(x%ch1 /= "hel")                              error stop 21_4

          type is(B(*,*))
             if(x%l1 /= 3)                                  error stop 22_4
             if(x%ch1%len /= len(x%ch1) .or. x%ch1%len /=3) error stop 23_4
             if(x%ch1 /= "hel")                              error stop 24_4
             if(x%l2 /= 3)                                  error stop 25_4
             if(x%ch2%len /= len(x%ch2) .or. x%ch2%len /=3) error stop 26_4
             if(x%ch2 /= "est")                             error stop 27_4

          class default
             error stop 102_4
       end select

    end subroutine

end
