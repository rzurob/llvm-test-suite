!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : arrsec2_datatype00.f
!*                               arrsec1_datatype00.f)
!*  DATE                       : Oct 13, 2008
!*  ORIGIN                     : XLF Development
!*  PRIMARY FUNCTIONS TESTED   : Array sections
!*  DESCRIPTION
!*
!*      Array component of integer type
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

    module m
      type A(n)
        integer, len :: n
        integer h(n)
        integer i(n)
        integer j(n)
      end type
    end module

    use m
    integer, parameter :: NA=4
    type(A(:)), allocatable :: a1(:)
    allocate (A(4) :: a1(NA))

    a1%h(4) = [(i, i=1,NA)]
    a1%i(2) = [(i, i=1,NA)]
    a1%j(1) = [(i, i=1,NA)]

    print *, a1(:)%h(4)
    print *, a1(:)%i(2)
    print *, a1(:)%j(1)
    end
