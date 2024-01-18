!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 22 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SPREAD(SOURCE,DIM,NCOPIES)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.114
!*  2. SPREAD AS INITIALIZATION EXPRESSION
!*  3. USE SPREAD IN STRUCTURE CONSTRUCTOR
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A(l1)
    integer,len   :: l1=2
    character(l1) :: c(l1)
  end type
  type B(k1)
    integer,kind :: k1=4
    integer(k1)  :: i(2)
  end type

  type C(k2,l2)
    integer,kind :: k2
    integer,len  :: l2
    type(A(l2))  :: a1(2)=[a(2)(spread("c",1,2)), a(2)(spread("d",1,2))]
    type(B(k2))  :: b1(2)=[b(4)(spread(-1,1,2)), b(4)(spread(-2,1,2))]

  end type
end module

program spreadAsInitExp02
  use m
  implicit none

  integer :: i,j

  type(c(4,2)) :: c1(2) = [c(4,2)(),c(4,2)()]

  type(c(4,2)),parameter :: c2 = &
          c(4,2)(spread( a(2)( spread("e",1,2) ),1,2), &
                 spread( b(4)( spread(-3,1,2) ),1,2) )

  if(c1%k2 /= 4)                                   error stop 10_4
  if(c1%l2 /= 2)                                   error stop 11_4
  do i=1,2
      if(c1(i)%a1%l1 /= 2)                         error stop 12_4
      if(c1(i)%b1%k1 /= 4)                         error stop 13_4
         if(any(c1(i)%a1(1)%c /= "c"))             error stop 14_4
         if(any(c1(i)%a1(2)%c /= "d"))             error stop 15_4
         if(any(c1(i)%b1(1)%i /= -1))              error stop 16_4
         if(any(c1(i)%b1(2)%i /= -2))              error stop 17_4
  end do

  if(c2%k2 /= 4)                                   error stop 18_4
  if(c2%l2 /= 2)                                   error stop 19_4
  if(c2%a1%l1 /= 2)                                error stop 20_4
  if(c2%b1%k1 /= 4)                                error stop 21_4
  if(any(c2%a1(1)%c /= "e"))                       error stop 22_4
  if(any(c2%a1(2)%c /= "e"))                       error stop 23_4
  if(any(c2%b1(1)%i /= -3))                        error stop 24_4
  if(any(c2%b1(2)%i /= -3))                        error stop 25_4

end program
