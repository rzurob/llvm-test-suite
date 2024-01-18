!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2009-11-05
!*
!*  DESCRIPTION                : miscellaneous (defect 371449)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
  type base(k,l)
     integer(2),kind :: k
     integer(1),len :: l
     integer(k) :: i(k-1:k+1)
     character(l) :: c(l-1:l+1)
  end type
end module

program dtParameterInquiryAssociate02
  use m
  implicit none

  type(base(2,:)),allocatable :: b1(:)
  character(:), allocatable :: str(:)

  allocate(base(2,3) :: b1(2))

  b1(1)=base(2,3)(i=[1,2,3],c=['abcd','efgh','ijkl'])
  b1(2)=base(2,3)(i=[-1,-2,-3],c=['red  ','blue ','green'])

  str = b1(1)%c

  if (.not. allocated (str)) stop 1

  if (len(str) /= 3) stop 2
  if (size(str) /= 3) stop 3
  if (any (str /= ['abc','efg','ijk'])) stop 4
end
