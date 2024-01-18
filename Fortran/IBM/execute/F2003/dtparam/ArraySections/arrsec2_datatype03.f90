!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : arrsec2_datatype03.f
!*                               arrsec1_datatype03.f)
!*  DATE                       : Oct 13, 2008
!*  ORIGIN                     : XLF Development
!*  PRIMARY FUNCTIONS TESTED   : Array sections
!*  DESCRIPTION
!*
!*      Array component of logical type
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

    module m
      type A(n)
        integer, len :: n
        logical h(n)
        logical i(n)
        logical j(n)
      end type
    end module

    use m
    integer, parameter :: NA=4
    type(A(:)), allocatable :: a1(:)
    allocate (A(4) :: a1(NA))

    a1%h(4) = .TRUE.
    a1%i(2) = .TRUE.
    a1%j(1) = .TRUE.

    print *, a1(:)%h(4)
    print *, a1(:)%i(2)
    print *, a1(:)%j(1)
    end
