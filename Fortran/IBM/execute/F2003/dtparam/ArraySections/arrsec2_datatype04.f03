!*******************************************************************************
!*  ============================================================================
!*
!*                               arrsec1_datatype04.f)
!*  DATE                       : Oct 13, 2008
!*  PRIMARY FUNCTIONS TESTED   : Array sections
!*  DESCRIPTION
!*
!*      Array component of character type
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

    module m
      type A(n)
        integer, len :: n
        character h(n)
        character i(n+2)
        character j(n+4)
      end type
    end module

    use m
    integer, parameter :: NA=4
    type(A(:)), allocatable :: a1(:)
    allocate (A(4) :: a1(NA))

    a1%h(4) = 'h'
    a1%i(2) = 'i'
    a1%j(1) = 'j'

    print *, a1(:)%h(4)
    print *, a1(:)%i(2)
    print *, a1(:)%j(1)
    end
