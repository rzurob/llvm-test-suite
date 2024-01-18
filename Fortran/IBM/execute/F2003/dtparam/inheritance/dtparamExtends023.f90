!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/12/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: Empty types with type parameters only.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k1, k2)
        integer, kind :: k1, k2 = 4
    end type

    type, extends(base) :: child (l1, l2)
        integer, len :: l1, l2 = 20
    end type

    type(child(k1=4, k2=8, l1=10)) c1_m
end module

program dtparamExtends023
use m
    type, extends(child) :: gen3 (k3, l3)
        integer, kind :: k3 = 8
        integer, len  :: l3 = 10
    end type

    type (gen3(8,8, 20, 10)) :: g1(2)
    type (gen3 (k1=4, l1=15)) :: g2(0:10, 2)

    !! verify that all type parameter values are set

    if ((c1_m%k1 /= 4) .or. (c1_m%k2 /= 8) .or. (c1_m%l1 /= 10) .or. &
        (c1_m%l2 /= 20))    error stop 1_4

    if ((g1%k1 /= 8) .or. (g1%k2 /= 8) .or. (g1%k3 /= 8))   error stop 2_4
    if ((g1%l1 /= 20) .or. (g1%l2 /= 10) .or. (g1%l3 /= 10)) error stop 3_4

    if ((g2%k1 /= 4) .or. (g2%k2 /= 4) .or. (g2%k3 /= 8)) error stop 4_4
    if ((g2%l1 /= 15) .or. (g2%l2 /= 20) .or. (g2%l3 /= 10)) error stop 5_4
end
