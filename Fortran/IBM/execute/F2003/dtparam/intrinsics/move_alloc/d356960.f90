!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d356960.f
!*
!*  DATE                       : Oct. 1 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. DEFECT 356960
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type A(l1)
       integer,len :: l1
       character(l1),allocatable :: c1(:)
    end type
    type B(l2)
       integer,len :: l2
       type(A(l2)),allocatable :: a1(:)
    end type
end module

program d356960
  use m
  implicit none

  type(B(3)),allocatable :: b1(:)
  type(B(:)),allocatable :: b2(:)

  allocate(b1(2))

  b1(1)%a1=[A(3)(["123","456"]),A(3)(["aaa","bbb"])]
  b1(2)%a1=[A(3)(["000","111"]),A(3)(["ccc","ddd"])]

  print *,b1(1)%a1%l1
  print *,b1(2)%a1%l1
  print *,b1(1)%a1(1)%c1%len,len(b1(1)%a1(1)%c1)
  print *,b1(2)%a1(1)%c1%len,len(b1(2)%a1(1)%c1)
  print *,b1(1)%a1(1)%c1,b1(1)%a1(2)%c1
  print *,b1(2)%a1(1)%c1,b1(2)%a1(2)%c1

  call move_alloc(b1,b2)
  if (allocated(b1)) error stop 1
  if (.not. allocated(b2)) error stop 2

  print *,b2(1)%a1%l1
  print *,b2(2)%a1%l1
  print *,b2(1)%a1(1)%c1%len,len(b2(1)%a1(1)%c1)
  print *,b2(2)%a1(1)%c1%len,len(b2(2)%a1(1)%c1)
  print *,b2(1)%a1(1)%c1,b2(1)%a1(2)%c1
  print *,b2(2)%a1(1)%c1,b2(2)%a1(2)%c1

end program
