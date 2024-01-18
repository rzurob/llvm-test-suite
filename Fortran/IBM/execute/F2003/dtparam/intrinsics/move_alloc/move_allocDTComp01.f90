!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : move_allocDTComp01.f
!*
!*  DATE                       : Oct. 1 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC(FROM,TO)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.82
!*  2. DERIVED TYPE HAS DT ARRAY COMPONENT WHICH HAS CHARACTER ARRAY COMPONENT,THEY ARE ALL ALLOCATABLE
!*  3. CALL MOVE_ALLOC IN MAIN PROGRAM
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type A(l1)
       integer,len :: l1
       character(l1),allocatable :: c1(:)
    end type
    type B(l2)
       integer,len :: l2
       type(A(l2+l2)),allocatable :: a1(:)
    end type
end module

program move_allocDTComp01

  use m
  implicit none

  type(B(3)),allocatable :: b1(:)
  type(B(:)),allocatable :: b2(:)
  class(*),allocatable   :: b3(:)

  allocate(b1(2))

  b1(1)%a1=[A(6)(["123","456"]),A(6)(["aaa","bbb"])]
  b1(2)%a1=[A(6)(["000","111"]),A(6)(["ccc","ddd"])]

  call move_alloc(b1,b2)

  if(allocated(b1))                              error stop 10_4
  if(.not. allocated(b2))                        error stop 11_4
  if(b2%l2 /= 3)                                 error stop 12_4
  if(b2%l2 /= 3)                                 error stop 13_4
  if(b2(1)%a1%l1 /= 6)                           error stop 14_4
  if(b2(2)%a1%l1 /= 6)                           error stop 15_4
  if(b2(1)%a1(1)%c1%len /= len(b2(1)%a1(1)%c1)) error stop 116_4
  if(b2(1)%a1(1)%c1%len /= 6)                     error stop 16_4
  if(b2(2)%a1(1)%c1%len /= len(b2(2)%a1(1)%c1)) error stop 117_4
  if(b2(2)%a1(1)%c1%len /= 6)                    error stop 17_4
  if(any(b2(1)%a1(1)%c1 /= ["123","456"]))       error stop 18_4
  if(any(b2(1)%a1(2)%c1 /= ["aaa","bbb"]))       error stop 19_4
  if(any(b2(2)%a1(1)%c1 /= ["000","111"]))       error stop 20_4
  if(any(b2(2)%a1(2)%c1 /= ["ccc","ddd"]))       error stop 21_4

  if(allocated(b1))      deallocate(b1)
  allocate(b1(2))
  b1(1)%a1=[A(6)(["123","456"]),A(6)(["aaa","bbb"])]
  b1(2)%a1=[A(6)(["000","111"]),A(6)(["ccc","ddd"])]

  call move_alloc(b1,b3)
  if(allocated(b1))                                   error stop 22_4
  if(.not. allocated(b3))                             error stop 23_4
  select type(b3)
     type is(B(*))
       if(b3%l2 /= 3)                                 error stop 24_4
       if(b3%l2 /= 3)                                 error stop 25_4
       if(b3(1)%a1%l1 /= 6)                           error stop 26_4
       if(b3(2)%a1%l1 /= 6)                           error stop 27_4
       if(b3(1)%a1(1)%c1%len /= len(b3(1)%a1(1)%c1)) error stop 107_4
       if(b3(1)%a1(1)%c1%len /= 6)                    error stop 28_4
       if(b3(2)%a1(1)%c1%len /= len(b3(2)%a1(1)%c1)) error stop 108_4
       if(b3(2)%a1(1)%c1%len /= 6)                    error stop 29_4
       if(any(b3(1)%a1(1)%c1 /= ["123","456"]))       error stop 30_4
       if(any(b3(1)%a1(2)%c1 /= ["aaa","bbb"]))       error stop 31_4
       if(any(b3(2)%a1(1)%c1 /= ["000","111"]))       error stop 32_4
       if(any(b3(2)%a1(2)%c1 /= ["ccc","ddd"]))       error stop 33_4
     class default
        error stop 100_4
  end select

end program

