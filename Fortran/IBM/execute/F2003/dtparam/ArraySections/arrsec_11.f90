!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArraySections/arrsec_11.f
!*  PROGRAMMER                 : Gaby Baghdadi
!*  DATE                       : Sep 23, 2008
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Feature Number 353925
!*
!*  DESCRIPTION:
!*
!*  6.2.2.3 Array sections
!*  Array section assignment of array of param der types (PDT).
!*  PDT is allocatable, deferred-shape, extends another, both with integer array
!*  components with len-type param.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base(n1)
        integer, len :: n1
         integer :: x
    end type

    type, extends(base) :: child(n2)
        integer, len :: n2
        integer :: y(n2)
    end type
end module

use m
type(child(:,:)), allocatable :: carr(:,:)
allocate(child(3,4) :: carr(3,2))

carr(2:,:2)%x = reshape([1,2,3,4],[2,2])
carr(1:2,:)%y(1) = reshape([1,2,3,4],[2,2])
print *,carr(3:2:-1,1:2)%x
print *,carr(2:3,2:1:-1)%x
carr(2:,2:1:-1)%x = carr(3:2:-1,:)%x
carr(2:1:-1,2:1:-1)%y(1) = carr(1:2,:)%y(1)
print *,carr(2:3,2:1:-1)%x
print *,carr(1:2,:)%y(1)

end
