! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/11/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Deferred type-parameters in
!                               declaration-type-spec: implicit statement.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (l)
        integer, len :: l

        character(l), allocatable :: names(:)
    end type
end module

program deferdparamDTSpec009
use m
    implicit type(base(l=:)) (x-y), class(base(:)) (z)
    allocatable x1, z1(:,:)
    pointer x2(:)
    type (base(l=:)), pointer :: z2

    allocate (base(20) ::x1)
    allocate (base(10) :: x2(100))
    allocate (base(30) :: z1(10, 10))

    z2 => x2(20)

    allocate(z2%names(2), source=(/'x2(20):1', 'x2(20):2'/))

    allocate(x1%names(0:5), source=(/('x1 : '//char(ichar('0') + i), i=0,5)/))

    x2(21) = x2(20)

    allocate (z1(5,5)%names(10))
    z1(5,5)%names = 'xlftest'

    print *, x1%names
    print *, x2(21)%names
    print *, z1(5,5)%names
end
