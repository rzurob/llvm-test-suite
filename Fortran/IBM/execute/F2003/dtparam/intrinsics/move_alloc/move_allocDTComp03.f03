!*********************************************************************
!*  ===================================================================
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
!*  2. DERIVED TYPE HAS DT POINTER COMPONENT WHICH HAS CHARACTER SCALAR COMPONENT
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type A(l1)
       integer,len :: l1
       character(l1) :: c1
    end type
    type B(l2)
       integer,len :: l2
       type(A(l2+l2)),pointer :: a1=>null()
    end type

end module

program move_allocDTComp03

  use m
  implicit none

  type(B(3)),allocatable :: b1(:)
  type(B(:)),allocatable :: b2(:)
  class(*),allocatable   :: b3(:)
  type(A(6)),target      :: a1(3)

  a1=[A(6)("xlf"),A(6)("test"),A(6)("team")]

  allocate(b1(4:6))
  b1(4)%a1=>a1(1)
  b1(5)%a1=>a1(2)
  b1(6)%a1=>a1(3)

  call move_alloc(b1,b2)

  if(allocated(b1))                                            error stop 10
  if(.not. allocated(b2))                                      error stop 11
  if(b2%l2 /= 3)                                               error stop 12
  if(lbound(b2,1) /= 4)                                        error stop 13
  if(ubound(b2,1) /= 6)                                        error stop 14
  if(b2(5)%a1%l1 /= 6)                                         error stop 15
  if(b2(5)%a1%c1%len /= len(b2(5)%a1%c1)) error stop 106
  if(b2(5)%a1%c1%len /= 6)                              error stop 16
  if(b2(4)%a1%c1 /= "xlf")                                     error stop 17
  if(b2(5)%a1%c1 /= "test")                                    error stop 18
  if(b2(6)%a1%c1 /= "team")                                    error stop 19

  if(allocated(b1))  deallocate(b1)
  allocate(b1(4:6))
  b1(4)%a1=>a1(1)
  b1(5)%a1=>a1(2)
  b1(6)%a1=>a1(3)

  call move_alloc(b1,b3)

  if(allocated(b1))                                            error stop 20
  if(.not. allocated(b3))                                      error stop 21
  select type(b3)
     type is(B(*))
       if(b3%l2 /= 3)                                          error stop 22
       if(lbound(b3,1) /= 4)                                   error stop 23
       if(ubound(b3,1) /= 6)                                   error stop 24
       if(b3(5)%a1%l1 /= 6)                                    error stop 25
       if(b3(5)%a1%c1%len /= len(b3(5)%a1%c1)) error stop 126
       if(b3(5)%a1%c1%len /= 6)                            error stop 26
       if(b3(4)%a1%c1 /= "xlf")                                error stop 27
       if(b3(5)%a1%c1 /= "test")                               error stop 28
       if(b3(6)%a1%c1 /= "team")                               error stop 29
  end select

  contains
     subroutine sub(arg1,arg2,len)
         integer,intent(in) :: len
         type(B(len)),allocatable :: arg1(:)
         class(*),allocatable :: arg2(:)

         call move_alloc(arg1,arg2)
     end subroutine
end program

