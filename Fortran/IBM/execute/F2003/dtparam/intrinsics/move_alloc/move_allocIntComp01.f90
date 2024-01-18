!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : move_allocIntComp01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Sept. 29 2008 
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
!*  2. COMPONENT IS INTEGER ALLOCATBLE ARRAY OR UNLIMITED POLYMORPHIC
!*  3. FROM AND TO ARE DERIVED TYPE OR ITS COMPONENTS 
!*  4. FROM IS ZERO SIZE ARRAY
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k,l)
      integer,kind :: k
      integer,len  :: l
      integer(k),allocatable   :: i1(:)
      integer(k),allocatable   :: i2(:) 
      class(*),allocatable     :: i3(:)
   end type
end module

program move_allocIntComp01

  use m
  implicit none

  type(dtp(4,3)),allocatable :: dtp1
  type(dtp(4,:)),allocatable :: dtp2
  class(dtp(4,:)),allocatable:: dtp3

  type(dtp(4,3)),allocatable :: dtp4(:)
  type(dtp(4,:)),allocatable :: dtp5(:)

  allocate(dtp1,source=dtp(4,3)([-1,0,1],null(),null()))

  allocate(dtp1%i2(-3:-1),source=2*dtp1%i1)
  allocate(dtp1%i3(0:2),source=3*dtp1%i1)

  if(.not. allocated(dtp1%i2))                     error stop 8_4
  if(.not. allocated(dtp1%i3))                     error stop 9_4
  if(lbound(dtp1%i2,1) /= -3)                      error stop 10_4
  if(ubound(dtp1%i2,1) /= -1)                      error stop 11_4
  if(any(dtp1%i2 /= [-2,0,2]))                     error stop 12_4
  select type(x=>dtp1%i3)
     type is(integer(4))
        if(lbound(x,1) /= 0)                       error stop 13_4
        if(ubound(x,1) /= 2)                       error stop 14_4
        if(any(x /= [-3,0,3]))                     error stop 15_4
     class default
        error stop 100_4
  end select 

  call move_alloc(from=dtp1%i1,to=dtp1%i2)

  if(allocated(dtp1%i1))                           error stop 16_4
  if(.not. allocated(dtp1%i3))                     error stop 17_4
  if(lbound(dtp1%i2,1) /= 1)                       error stop 18_4
  if(ubound(dtp1%i2,1) /= 3)                       error stop 19_4
  if(any(dtp1%i2 /= [-1,0,1]))                     error stop 20_4

  !--- zero size array--!
  allocate(dtp1%i1(0:-2),source=11)  
  call move_alloc(from=dtp1%i1,to=dtp1%i3)

  if(allocated(dtp1%i1))                           error stop 21_4
  if(.not. allocated(dtp1%i3))                     error stop 22_4

  select type(x=>dtp1%i3)
     type is(integer(4))
        if(lbound(x,1) /= 1)                       error stop 23_4
        if(ubound(x,1) /= 0)                       error stop 24_4
     class default
         error stop 101_4
  end select

  call move_alloc(from=dtp1,to=dtp2)

  if(allocated(dtp1))                              error stop 25_4
  if(.not. allocated(dtp2))                        error stop 26_4
  if(dtp2%k /= 4)                                  error stop 27_4
  if(dtp2%l /= 3)                                  error stop 28_4
  if(allocated(dtp2%i1))                           error stop 29_4
  if(.not. allocated(dtp2%i2))                     error stop 30_4
  if(.not. allocated(dtp2%i3))                     error stop 31_4
 
  dtp2%i1=[1,2,3]
 
  call move_alloc(dtp2,dtp3)

  if(allocated(dtp2))                              error stop 32_4
  if(.not. allocated(dtp3))                        error stop 33_4
  select type(dtp3)
    type is(dtp(4,*))
    if(.not. allocated(dtp3%i1))                   error stop 34_4
    if(.not. allocated(dtp3%i2))                   error stop 35_4
    if(.not. allocated(dtp3%i3))                   error stop 36_4
    if(dtp3%l /= 3)                                error stop 37_4 
    if(any(dtp3%i1 /= [1,2,3]))                    error stop 38_4
    if(lbound(dtp3%i1,1) /= 1)                     error stop 39_4
    if(ubound(dtp3%i1,1) /= 3)                     error stop 40_4
    if(lbound(dtp3%i2,1) /= 1)                     error stop 41_4
    if(ubound(dtp3%i2,1) /= 3)                     error stop 42_4
    if(any(dtp3%i2 /= [-1,0,1]))                   error stop 43_4
    if(lbound(dtp3%i3,1) /= 1)                     error stop 44_4
    if(ubound(dtp3%i3,1) /= 0)                     error stop 45_4 
   class default
     error stop 102_4
  end select 
  
  !--- zero size array---!
  allocate(dtp4(3:0),source=dtp3)
  
  if(.not. allocated(dtp4))                        error stop 46_4
  
  call move_alloc(dtp4,dtp5)

  if(.not. allocated(dtp5))                        error stop 47_4  
  if(allocated(dtp4))                              error stop 48_4
  if(dtp5%l /= 3)                                  error stop 49_4
  if(lbound(dtp5,1) /= 1)                          error stop 50_4
  if(ubound(dtp5,1) /= 0)                          error stop 51_4

  
end program

