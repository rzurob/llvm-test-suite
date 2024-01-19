!*******************************************************************************
!*  ============================================================================
!*
!*                               arrsec1_datatype07.f)
!*  DATE                       : Oct 13, 2008
!*  PRIMARY FUNCTIONS TESTED   : Array sections
!*  DESCRIPTION
!*
!*      Array component of derived type
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

    module m
      type B
        integer :: e(2) = 0
      end type
      type A(n)
        integer, len :: n
        type(B) :: c(n)
        type(B) :: d(n+2)
      end type
    end module

    use m

    integer, parameter :: NA=5, NB=5
    type(A(:)), allocatable :: a1(:)
    type(B) :: b1(NB)
    type(B) :: b2(NB)

    allocate (A(4) :: a1(NA))
    b1%e(1) = 1
    b2%e(2) = 2
    a1%c(3) = b1
    a1%d(4) = b2

    print *, a1(:)%d(4)
    end
