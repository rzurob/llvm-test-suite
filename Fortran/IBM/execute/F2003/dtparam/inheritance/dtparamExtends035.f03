! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/22/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: private keyword has no effects on type
!                               parameters' accessibilities.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, private :: base (k,l)
        integer, kind :: k
        integer, len :: l = 20

        private

        integer(k) :: id = 0
        character(l) :: name = ''
    end type

    type, extends(base) :: child (n)
        integer, len :: n

        private

        real(k), allocatable :: data
        complex(k) :: cx(n) = (0., 0.0)
    end type
end module

program dtparamExtends035
use m
    type (child(4, 15, 25)), allocatable :: c1(:)

    class (child (8, 10, 20)), pointer :: c2

    type (child(8, n=50)) c3(10)

    allocate (c1(100), c2)

    !! verify the type paramaters for c1, c2 and c3

    if ((c1%k /= 4) .or. (c1%l /= 15) .or. (c1%n /= 25)) error stop 1_4
    if ((c2%k /= 8) .or. (c2%l /= 10) .or. (c2%n /= 20)) error stop 2_4
    if ((c3%k /= 8) .or. (c3%l /= 20) .or. (c3%n /= 50)) error stop 3_4
end
