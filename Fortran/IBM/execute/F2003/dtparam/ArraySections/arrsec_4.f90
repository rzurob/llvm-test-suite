!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArraySections/arrsec_4.f
!*  PROGRAMMER                 : Gaby Baghdadi
!*  DATE                       : Jul. 09, 2008
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Feature Number 353925
!*
!*  DESCRIPTION:
!*
!*  6.2.2.3.1 Subscript triplet
!*  A subscript triplet designates a regular sequence of subscripts consisting 
!*  of zero or more subscript values. The third expression in the subscript 
!*  triplet is the increment between the subscript values and is called the 
!*  stride. 
!*  The subscripts and stride of a subscript triplet are optional. An omitted 
!*  first subscript in a subscript triplet is equivalent to a subscript whose 
!*  value is the lower bound for the array and an omitted second subscript is 
!*  equivalent to the upper bound. An omitted stride is equivalent to a stride 
!*  of 1.
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module m
    type base(n1)
        integer, len :: n1
        integer :: arr(n1)
        integer :: id
    end type

    type dt1(n1)
        integer, len :: n1
        integer :: id1
        integer :: arr1(n1)
        type(base(n1)) :: bc(n1)
    end type

    type, extends(dt1) :: dt2(n2)
        integer, len :: n2
        integer :: arr2(n2)
        integer :: id2
    end type
end module

use m
integer, parameter :: N = 3
!type(dt2(N,N)) :: dta(N)                ! OK
type(dt2(N,N)) :: dtb(N:N*2)
! dta as deferred-shape array cause prints below to seg fault (defect 354613)
type(dt2(:,:)), allocatable :: dta(:)
allocate(dt2(N,N) :: dta(N))

dta(1)%bc%id = (/(i,i=1,N)/)
print *,'dta(1)%bc%id=',dta(1)%bc%id
print *,'dta(1)%bc(:)%id=',dta(1)%bc(:)%id
print *,'dta(1)%bc(:N)%id=',dta(1)%bc(:N)%id
print *,'dta(1)%bc(1::1)%id=',dta(1)%bc(1::1)%id
print *,'dta(1)%bc(1:)%id=',dta(1)%bc(1:)%id

! any of these 4 cause prints below to ICE (defect 354611)
dtb(:)%bc(N)%id = (/(i,i=N,N*2)/)
print *,'dtb(:)%bc(N)%id=',dtb(:)%bc(N)%id        
print *,'dtb(:2*N)%bc(N)%id=',dtb(:2*N)%bc(N)%id
print *,'dtb(N:)%bc(N)%id=',dtb(N:)%bc(N)%id
print *,'dtb(N::1)%bc(N)%id=',dtb(N::1)%bc(N)%id

end
