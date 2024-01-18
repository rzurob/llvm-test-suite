!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 7 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. DEFECT 357194
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A(l1)
    integer,len   :: l1
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

program d357194
  use m
  implicit none

  type(C(:)),allocatable :: from1

  allocate(from1,source=C(3)(a2=A(4)(),b1=B(2)(a1=A(8)()) ) )

  print *,from1%l3
  print *,from1%a2%l1
  print *,from1%b1%l2
  print *,from1%b1%a1%l1

end program

