!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d357837.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Oct. 22 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :  
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  1. DEFECT 357837
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

program d357837
  use m
  implicit none

  integer :: i
  type(C(4,2)) :: c1(2:3)=spread(c(4,2)(),1,2)

  if(c1%k2 /= 4)                                      error stop 10_4
  if(c1%l2 /= 2)                                      error stop 11_4
  do i=2,3
    if(any(c1(i)%a1%ch /= "x"))                       error stop 12_4
    if(any(c1(i)%b1%int /= 99))                       error stop 13_4
    if(c1(i)%a1%l1 /= 2)                              error stop 14_4
    if(c1(i)%b1%k1 /= 4)                              error stop 15_4
  end do
end program

