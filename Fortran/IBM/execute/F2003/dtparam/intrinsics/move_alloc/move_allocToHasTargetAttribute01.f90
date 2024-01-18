!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 2 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC(FROM,TO)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.82
!*  2. IF TO HAS THE TARGET ATTRIBUTE,ANY POINTER ASSOCIATED WITH FROM ON ENTRY MOVE_ALLOC BECOMES CORRESPONDINGLY ASSOCIATED WITH TO.
!*  3. FROM AND TO ARE SCALAR
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(k1,l1)
     integer,kind :: k1
     integer,len  :: l1
     integer(k1)   :: i1
     character(l1) :: c1
   end type
   type B(k2,l2)
      integer(8),kind :: k2=8
      integer(2),len  :: l2=4
      type(A(k2,l2))  :: a1
   end type
end module

program move_allocToHasTargetAttribute01

  use m
  implicit none

  class(*),pointer     :: point1=>null()
  type(B(8,:)),pointer :: point2=>null()

  type(B(8,4)),target,allocatable :: from1
  type(B(8,:)),target,allocatable :: to1

  from1=B(8,4)(a1=A(8,4)(i1=11,c1="xlf"))
  point1=>from1

  call move_alloc(from=from1,to=to1)

  if(allocated(from1))                                 error stop 10_4
  if(.not. allocated(to1))                             error stop 11_4

  if(.not. associated(point1,to1))                     error stop 12_4
  select type(point1)
      type is(B(8,*))
         if(point1%k2 /= 8)                            error stop 13_4
         if(point1%l2 /= 4)                            error stop 14_4
         if(point1%a1%k1 /= 8)                         error stop 15_4
         if(point1%a1%l1 /= 4)                         error stop 16_4
         if(point1%a1%i1%kind /= 8)                    error stop 17_4
         if(point1%a1%c1%len /= 4)                     error stop 18_4
         if(point1%a1%i1 /= 11)                        error stop 19_4
         if(point1%a1%c1 /= "xlf")                     error stop 20_4
      class default
         error stop 100_4
  end select

  from1= B(8,4)(a1=A(8,4)(i1=-11,c1="test"))

  allocate(point2,source=from1)

  call move_alloc(from1,to1)

  if(allocated(from1))                                 error stop 21_4
  if(.not. allocated(to1))                             error stop 22_4
  if(.not. associated(point2))                         error stop 23_4
  if(point2%k2 /= 8)                                   error stop 24_4
  if(point2%l2 /= 4)                                   error stop 25_4
  if(point2%a1%k1 /= 8)                                error stop 26_4
  if(point2%a1%l1 /= 4)                                error stop 27_4
  if(point2%a1%i1%kind /= 8)                           error stop 28_4
  if(point2%a1%c1%len /= 4)                            error stop 29_4
  if(point2%a1%i1 /= -11)                              error stop 30_4
  if(point2%a1%c1 /= "test")                           error stop 31_4

end program

