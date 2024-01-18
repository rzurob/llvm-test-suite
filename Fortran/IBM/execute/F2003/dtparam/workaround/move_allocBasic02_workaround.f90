!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : move_allocBasic02.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Oct. 7 2008 
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
!*  2. DERIVED TYPE HAS MULTIPLE DERIVED TYPE COMPONENTS
!*  3. TYPE PARAMETER FOR FROM AND TO ARE DEFERRED
!*  4. DEFECT 357194
!234567890123456789012345678901234567890123456789012345678901234567890

module m
  type A(l1)
    integer,len   :: l1
    integer :: i1(l1)
    integer :: i2(l1)
  end type
  type B(l2)
    integer,len   :: l2
    type(A(2*(l2+l2))) :: a1
  end type
  type C(l3)
    integer,len   :: l3
    type(A(l3+1)) :: a2
    type(B(l3-1)) :: b1
  end type 
end module

program move_allocBasic02

  use m
  implicit none

  type(C(3)),allocatable :: from1
  type(C(3)),allocatable :: to1

  type(C(3)),allocatable :: from2(:)
  type(C(3)),allocatable :: to2(:)

  allocate(from1,source=C(3)(a2=A(4)(i1=1,i2=-1), & 
                             b1=B(2)(a1=A(8)(i1=2,i2=-2)) ) )

  allocate(from2(3:4),source= [C(3)(a2=A(4)(i1=3,i2=-3),b1=B(2)(a1=A(8)(i1=4,i2=-4))), C(3)(a2=A(4)(i1=5,i2=-5),b1=B(2)(a1=A(8)(i1=6,i2=-6 ) ) )] )


  if(from1%l3 /= 3)                          error stop 10_4
  if(from1%a2%l1 /= 4)                       error stop 11_4
  if(any(from1%a2%i1 /= 1))                  error stop 12_4
  if(any(from1%a2%i2 /= -1))                 error stop 13_4
  if(from1%b1%l2 /= 2)                       error stop 14_4
  if(from1%b1%a1%l1 /= 8)                    error stop 15_4
  if(any(from1%b1%a1%i1 /= 2))               error stop 16_4
  if(any(from1%b1%a1%i2 /= -2))              error stop 17_4

  call move_alloc(from1,to1)      

  if(allocated(from1))                       error stop 18_4
  if(.not. allocated(to1))                   error stop 19_4

  if(to1%l3 /= 3)                            error stop 20_4
  if(to1%a2%l1 /= 4)                         error stop 21_4
  if(any(to1%a2%i1 /= 1))                    error stop 22_4
  if(any(to1%a2%i2 /= -1))                   error stop 23_4
  if(to1%b1%l2 /= 2)                         error stop 24_4
  if(to1%b1%a1%l1 /= 8)                      error stop 25_4
  if(any(to1%b1%a1%i1 /= 2))                 error stop 26_4
  if(any(to1%b1%a1%i2 /= -2))                error stop 27_4
                 
   allocate(C(3):: to2(-200:200))

   if(lbound(to2,1) /= -200)                 error stop 28_4
   if(ubound(to2,1) /= 200)                  error stop 29_4
      
   call move_alloc(from2,to2)

  if(allocated(from2))                       error stop 30_4
  if(.not. allocated(to2))                   error stop 31_4

  if(lbound(to2,1) /= 3)                     error stop 32_4
  if(ubound(to2,1) /= 4)                     error stop 33_4

  if(to2%l3 /= 3)                            error stop 34_4
  if(to2%a2%l1 /= 4)                         error stop 35_4
  if(any(to2(3)%a2%i1 /= 3))                 error stop 36_4
  if(any(to2(3)%a2%i2 /= -3))                error stop 37_4
  if(any(to2(4)%a2%i1 /= 5))                 error stop 38_4
  if(any(to2(4)%a2%i2 /= -5))                error stop 39_4
  if(to2%b1%l2 /= 2)                         error stop 40_4
  if(to2%b1%a1%l1 /= 8)                      error stop 41_4
  if(any(to2(3)%b1%a1%i1 /= 4))              error stop 42_4
  if(any(to2(3)%b1%a1%i2 /= -4))             error stop 43_4
  if(any(to2(4)%b1%a1%i1 /= 6))              error stop 44_4
  if(any(to2(4)%b1%a1%i2 /= -6))             error stop 45_4 
   
end program
