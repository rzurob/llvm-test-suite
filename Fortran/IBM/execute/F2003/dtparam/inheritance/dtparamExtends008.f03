! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/23/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Statement: An extended type has a scalar,
!                               nonpointer, nonallocatable parent component with
!                               the type and type parameters of the parent type.
!                               Case: the parent component is an empty type, but
!                               with type parameter defined.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type arrayTemplate (size)
        integer, len :: size
    end type
end module

module m1
use m
    type, extends(arrayTemplate) :: realArray (k)
        integer, kind :: k

        real(k) :: data(size)
        logical(k) isSet
    end type

    class (realArray(100, 8)), pointer :: ra1
end module


program dtparamExtends008
use m1
    type (realArray(200, 4)) ra2

    !! verify the parent component's type parameter
    if (ra2%arrayTemplate%size /= 200) error stop 1_4

    allocate (ra1)

    if (ra1%arrayTemplate%size /= 100) error stop 2_4
end
