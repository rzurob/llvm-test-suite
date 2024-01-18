!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArraySections/arrsec_14.f
!*  PROGRAMMER                 : Gaby Baghdadi
!*  DATE                       : Sep 26, 2008
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Feature Number 353925
!*
!*  DESCRIPTION:
!*
!234567890123456789012345678901234567890123456789012345678901234567890
  
module m
	type base(n)
		integer, len :: n
		integer :: id(n)
	end type
end module

use m
integer, parameter :: L=4, N=3
type (base(:)) ,allocatable :: dtarr(:)
allocate(base(L) :: dtarr(N))

dtarr(:)%id(L) = [(i,i=1,N)]
print *,dtarr%id(L)

dtarr(1:N) = foo1()
print *,dtarr%id(L)

dtarr(1:N) = foo2(dtarr(N:1:-1))
print *,dtarr%id(L)

contains
	function foo1 ( )
		type(base(L)) :: foo1(N)
		foo1 = [base(L) :: (base(L)(i),i=N,1,-1)]
	end function

	function foo2 ( arr )
		type(base(*)) :: arr(N)
		type(base(L)) :: foo2(N)
		foo2 = [base(L) :: arr(1:N)]
	end function
end
