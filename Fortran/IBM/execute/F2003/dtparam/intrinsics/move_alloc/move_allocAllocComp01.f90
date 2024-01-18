
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : move_allocAllocComp01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Oct. 9 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC(FROM,TO) 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.82
!*  2. COMPONENTS ARE ALLOCATABLE, SOME LENGTH PARAMETER ARE DEFERRED
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type first(l1)
     integer,len :: l1
     integer,allocatable  :: i1(:)
     integer,allocatable  :: i2(:)
   end type
   type second(l2)
     integer,len :: l2
     integer,allocatable  :: i3(:)
     type(first(:)),allocatable :: first1 
   end type
   type third(l3)
     integer,len  :: l3
     type(first(:)),allocatable  :: first2
     type(second(:)),allocatable :: second1
   end type
end module

program move_allocAllocComp01

  use m
  implicit none

  integer :: i

  type(third(:)),allocatable :: third1,from1,to1

  allocate(third(1) :: third1)

  allocate(first(3) :: third1%first2)
  allocate(second(3) :: third1%second1)

  allocate(third1%first2%i1(-3:-1),source=[1,2,3])

  allocate(third1%first2%i2(0:3),source=[4,5,6,7])

  allocate(third1%second1%i3(-9:-8),source=[-1,-2])
  allocate(first(6) :: third1%second1%first1)

  allocate(third1%second1%first1%i1(5),source=[ (-i,i=5,1,-1)] )
  allocate(third1%second1%first1%i2(3),source=[ (i,i=8,10)] )


  from1=third1 
  
  call move_alloc(from1,to1)
  
  if(allocated(from1))                       error stop 10_4
  if(.not. allocated(to1))                   error stop 11_4
  if(.not. allocated(to1%first2))            error stop 12_4
  if(.not. allocated(to1%first2%i1))         error stop 13_4
  if(.not. allocated(to1%first2%i2))         error stop 14_4
  if(.not. allocated(to1%second1))           error stop 15_4
  if(.not. allocated(to1%second1%i3))        error stop 16_4
  if(.not. allocated(to1%second1%first1))    error stop 17_4
  if(.not. allocated(to1%second1%first1%i1)) error stop 18_4
  if(.not. allocated(to1%second1%first1%i2)) error stop 19_4

  if(to1%l3 /= 1)                            error stop 20_4
  if(to1%first2%l1 /= 3)                     error stop 21_4
  if(to1%second1%l2 /= 3)                    error stop 22_4
  if(to1%second1%first1%l1 /= 6)             error stop 23_4

  if(lbound(to1%first2%i1,1) /= -3)          error stop 24_4
  if(ubound(to1%first2%i1,1) /= -1)          error stop 25_4
  if(lbound(to1%first2%i2,1) /= 0)           error stop 26_4
  if(ubound(to1%first2%i2,1) /= 3)           error stop 27_4

  if(any(to1%first2%i1 /= [1,2,3]))          error stop 28_4
  if(any(to1%first2%i2 /= [4,5,6,7]))        error stop 29_4

  if(lbound(to1%second1%i3,1) /= -9)         error stop 30_4
  if(ubound(to1%second1%i3,1) /= -8)         error stop 31_4

  if(any(to1%second1%i3 /= [-1,-2]))         error stop 32_4
  if(any(third1%second1%first1%i1 /= [-5,-4,-3,-2,-1]))       error stop 33_4
  if(any(third1%second1%first1%i2 /= [8,9,10]))               error stop 34_4 

end program

