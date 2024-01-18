!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArraySections/arrsec_4d.f
!*  PROGRAMMER                 : Gaby Baghdadi
!*  DATE                       : Jul. 09, 2008
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Feature Number 353925
!*
!*  DESCRIPTION:
!*
!*  6.2.2.3.1 Subscript triplet
!*  The stride shall not be zero
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
integer, parameter :: S = 0
integer :: vs
type(dt2(N,N)) :: dta(N)

dta(1)%bc%id = (/(i,i=1,N)/)
print *,dta(1)%bc(::S)%id
vs = 0
print *,dta(1)%bc(:N:vs)%id
print *,dta(1)%bc(1::0)%id

dta(:)%bc(N)%id = (/(i,i=1,N)/)
print *,dta(::0)%bc(N)%id		
print *,dta(:N:0)%bc(N)%id
print *,dta(N::S)%bc(N)%id
print *,dta(N::vs)%bc(N)%id

end
