!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : arrsec2_datatype01.f
!*  PROGRAMMER                 : Gaby Baghdadi (adopted from David Nichols' 
!*                               arrsec1_datatype01.f)
!*  DATE                       : Oct 13, 2008
!*  ORIGIN                     : XLF Development
!*  PRIMARY FUNCTIONS TESTED   : Array sections
!*  DESCRIPTION
!*
!*      Array component of real type
!*      
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

    module m
      type A(n)
        integer, len :: n
        real h(n) 
        real i(n) 
        real j(n) 
      end type
    end module

    use m
    integer, parameter :: NA=4
	real :: r
    type(A(:)), allocatable :: a1(:)
    allocate (A(4) :: a1(NA))

    a1%h(4) = [(r, r=1,NA)]
    a1%i(2) = [(r, r=1,NA)]
    a1%j(1) = [(r, r=1,NA)]

    print *, a1(:)%h(4)
    print *, a1(:)%i(2)
    print *, a1(:)%j(1)
    end
