!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : spreadAsInitExp01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Oct. 22 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : SPREAD(SOURCE,DIM,NCOPIES) 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.114
!*  2. SPREAD AS INITIALIZATION EXPRESSION
!*  3. INITIALIZE DERIVED TYPE COMPONENT AND DERIVED TYPE WITH SPREAD
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A(l1)
    integer,len   :: l1=2
    character(l1) :: ch="x"
  end type   
  type B(k1)
    integer,kind :: k1=4
    integer(k1)  :: int=99
  end type

  type C(k2,l2)
    integer,kind :: k2
    integer,len  :: l2
    type(A(l2))  :: a1(2)=spread(a(2)(),1,2)
    type(B(k2))  :: b1(2)=spread(b(k2)(),1,2)
  end type
end module

program spreadAsInitExp01
  use m
  implicit none

  integer :: i,j

  type(C(4,2)) :: c1(2:3)=spread(c(4,2)(),1,2)
  type(C(4,2)),parameter :: c2(2)= &
               spread(c(4,2)(a1=spread( a(2)(ch="ab"),1,2) , &
                             b1=spread( b(4)(int=11),1,2) ),1,2 ) 
  type(C(4,2)),allocatable :: c3(:)

  c3=c2

  if(lbound(c1,1) /= 2)                           error stop 10_4
  if(ubound(c1,1) /= 3)                           error stop 11_4
  if(c1%k2 /= 4)                                  error stop 12_4
  if(c1%l2 /= 2)                                  error stop 13_4
  do i=2,3
    do j=1,2
      if(c1(i)%a1(j)%l1 /= 2)                     error stop 14_4
      if(c1(i)%a1(j)%ch /= "x")                   error stop 15_4
      if(c1(i)%b1(j)%k1 /= 4)                     error stop 16_4
      if(c1(i)%b1(j)%int /= 99)                   error stop 17_4 
    end do   
  end do

  if(lbound(c3,1) /= 1)                           error stop 18_4
  if(ubound(c3,1) /= 2)                           error stop 19_4
  if(c3%k2 /= 4)                                  error stop 20_4
  if(c3%l2 /= 2)                                  error stop 21_4
  do i=1,2
    do j=1,2
      if(c3(i)%a1(j)%l1 /= 2)                     error stop 22_4
      if(c3(i)%a1(j)%ch /= "ab")                  error stop 23_4
      if(c3(i)%b1(j)%k1 /= 4)                     error stop 24_4
      if(c3(i)%b1(j)%int /= 11)                   error stop 25_4
    end do
  end do
end program
