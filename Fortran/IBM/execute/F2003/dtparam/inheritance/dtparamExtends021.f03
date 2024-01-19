! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/12/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: Type parameter names can be referenced via
!                               parent component
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type param (k1, l1)
        integer(2), kind :: k1
        integer(8), len  :: l1
    end type

    type base (k1, k2, l1, l2)
        integer, kind:: k1, k2
        integer, len :: l1, l2 = 10

        class (param(k1, l1)), pointer :: p1 => null()
        complex(k2) cx(l2)
    end type
end module

module m1
use m
    type, extends(base) :: child (k3, l3)
        integer, kind :: k3
        integer, len :: l3

        real(k3) data
        character(l3) name
    end type
end module

module m2
use m1
    type, extends(child) :: gen3 (k4, k5, l4)
        integer, kind :: k4, k5
        integer, len  :: l4

        integer(k4) id(l4)
        class (param(k5, l4)), allocatable :: p2
    end type

    class (gen3(k1=4, k2=4, k3=8, k4=4, k5=8, l1=10, l2=12, l3=20, l4=8)),&
                pointer :: g1_m
end module

program dtparamExtends021
use m2
    type (gen3(k1=8, k2=4, k3=8, k4=4, k5=8, l1=10, l2=2, l3=25, l4=16)) g1(2)

    allocate (g1_m)

    !! verify the type parameter values
    if ((g1_m%k1 /= 4) .or. (g1_m%k2 /= 4) .or. (g1_m%k3 /= 8) .or. &
        (g1_m%k4 /= 4) .or. (g1_m%k5 /= 8)) error stop 1_4

    if ((g1%k1 /= 8) .or. (g1%k2 /= 4) .or. (g1%k3 /= 8) .or. &
        (g1%k4 /= 4) .or. (g1%k5 /= 8))     error stop 2_4

    if ((g1_m%l1 /= 10) .or. (g1_m%l2 /= 12) .or. (g1_m%l3 /= 20) .or. &
        (g1_m%l4 /= 8))     error stop 3_4

    if ((g1%l1 /= 10) .or. (g1%l2 /= 2) .or. (g1%l3 /= 25) .or. &
        (g1%l4 /= 16))      error stop 4_4


    !! verify that the type parameter names can be accessed by the parent
    !components
    if ((g1_m%k1 /= g1_m%child%k1) .or. (g1_m%k1 /= g1_m%base%k1) .or. &
        (g1_m%k1 /= g1_m%child%base%k1))   error stop 5_4

    if ((g1_m%k2 /= g1_m%child%k2) .or. (g1_m%k2 /= g1_m%base%k2) .or. &
        (g1_m%k2 /= g1_m%child%base%k2))   error stop 6_4

    if ((g1_m%l1 /= g1_m%child%l1) .or. (g1_m%l1 /= g1_m%base%l1) .or. &
        (g1_m%l1 /= g1_m%child%base%l1))   error stop 7_4

    if ((g1_m%l2 /= g1_m%child%l2) .or. (g1_m%l2 /= g1_m%base%l2) .or. &
        (g1_m%l2 /= g1_m%child%base%l2))   error stop 8_4

    if ((g1_m%k3 /= g1_m%child%k3) .or. (g1_m%l3 /= g1_m%child%l3)) &
                error stop 9_4

    !!
    if ((g1%k1 /= g1%child%k1) .or. (g1%k1 /= g1%base%k1) .or. &
        (g1%k1 /= g1%child%base%k1))   error stop 15_4

    if ((g1%k2 /= g1%child%k2) .or. (g1%k2 /= g1%base%k2) .or. &
        (g1%k2 /= g1%child%base%k2))   error stop 16_4

    if ((g1%l1 /= g1%child%l1) .or. (g1%l1 /= g1%base%l1) .or. &
        (g1%l1 /= g1%child%base%l1))   error stop 17_4

    if ((g1%l2 /= g1%child%l2) .or. (g1%l2 /= g1%base%l2) .or. &
        (g1%l2 /= g1%child%base%l2))   error stop 18_4

    if ((g1%k3 /= g1%child%k3) .or. (g1%l3 /= g1%child%l3)) &
                error stop 19_4

end
