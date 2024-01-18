! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/31/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: C478; part2: If the type-parameter has a
!                               default value, then it's not required to have a
!                               corresponding type-parameter-spec if the value
!                               is that of the default value.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k = 4
        integer, len :: n = 20

        real(k) :: data (n) = -1.0
    end type
end module

program dtSpec003
use m
    type (base(n=10)) :: b1

    class (base(8)), pointer :: b2
    class (base), allocatable :: b3(:)
    class (base(n=:)), allocatable :: b4(:,:)

    allocate (b2, b3(100))
    allocate (base :: b4(2,2))

    !! verify the values for the type parameters
    if ((b1%k /= 4) .or. (b1%n /= 10)) error stop 1_4
    if ((b2%k /= 8) .or. (b2%n /= 20)) error stop 2_4
    if ((b3%k /= 4) .or. (b3%n /= 20)) error stop 3_4
    if ((b4%k /= 4) .or. (b4%n /= 20)) error stop 4_4
end
