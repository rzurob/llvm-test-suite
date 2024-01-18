!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : move_allocBasic03.f
!*
!*  DATE                       : Oct. 7 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC(FROM,TO)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.82
!*  2. TO IS POLYMORPHIC TYPE ,FROM IS CHILD TYPE
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
      integer,len :: l1
      integer :: i1(2*l1:2*(l1+1))
   end type
   type, extends(base) :: child(l2)
      integer,len :: l2
      integer     :: i2(l2-1:l2+1)
      type(base(l2)) :: b1
   end type
end module

program move_allocBasic03

  use m
  implicit none

  type(child(:,:)),allocatable :: from1
  class(base(:)),allocatable   :: to1
  type(base(2)),allocatable    :: base1

  allocate(base(2) :: base1)
  base1=base(2)(i1=[1,2,3])

  allocate(from1,source=child(1,2)(i1=[-2,-3,-4],i2=[-5,-6,-7] ,b1=base1) )

  if(from1%l1 /= 1)                                error stop 10_4
  if(from1%l2 /= 2)                                error stop 11_4
  if(any(from1%i1 /= [-2,-3,-4] ))                 error stop 12_4
  if(any(from1%i2 /= [-5,-6,-7] ))                 error stop 13_4
  if(from1%b1%l1 /= 2)                             error stop 14_4

  if(any(from1%b1%i1 /= [1,2,3] ))                 error stop 15_4
  if(lbound(from1%i1,1) /= 2)                      error stop 16_4
  if(ubound(from1%i1,1) /= 4)                      error stop 17_4
  if(lbound(from1%i2,1) /= 1)                      error stop 18_4
  if(ubound(from1%i2,1) /= 3)                      error stop 19_4
  if(lbound(from1%b1%i1,1) /= 4)                   error stop 20_4
  if(ubound(from1%b1%i1,1) /= 6)                   error stop 21_4

  call move_alloc(from1,to1)

  if(allocated(from1))                             error stop 22_4
  if(.not. allocated(to1))                         error stop 23_4

  select type(to1)
     type is(child(*,*))
     if(to1%l1 /= 1)                               error stop 24_4
     if(to1%l2 /= 2)                               error stop 25_4
     if(any(to1%i1 /= [-2,-3,-4] ))                error stop 26_4
     if(any(to1%i2 /= [-5,-6,-7] ))                error stop 27_4
     if(to1%b1%l1 /= 2)                            error stop 28_4
     if(any(to1%b1%i1 /= [1,2,3] ))                error stop 29_4
     if(lbound(to1%i1,1) /= 2)                     error stop 30_4
     if(ubound(to1%i1,1) /= 4)                     error stop 31_4
     if(lbound(to1%i2,1) /= 1)                     error stop 32_4
     if(ubound(to1%i2,1) /= 3)                     error stop 33_4
     if(lbound(to1%b1%i1,1) /= 4)                  error stop 34_4
     if(ubound(to1%b1%i1,1) /= 6)                  error stop 35_4
     class default
        error stop 100_4
  end select
end program
