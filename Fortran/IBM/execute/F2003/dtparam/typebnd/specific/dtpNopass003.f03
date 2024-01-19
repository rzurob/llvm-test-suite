! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/26/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type-bound procedures (Test that the
!                               nopass binding is inherited from base type.)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data
        character(n) :: name

        contains

        procedure, nopass, non_overridable :: equal4to4 => compareK4K4
        procedure, nopass :: equal8to4 => compareK8K4
    end type

    type, extends(base) :: child
        character :: rep(10+n)

        contains

        procedure, nopass :: equal8to8 => compareK8K8
    end type

    contains

    logical function compareK4K4 (b1, b2)
        class(base(4,*)), intent(in) :: b1, b2

        logical(4), external :: precision_r4

        compareK4K4 = precision_r4(b1%data, b2%data) .and. &
            (b1%name == b2%name)
    end function

    logical function compareK8K4 (b1, b2)
        class(base(8,*)), intent(in) :: b1
        class(base(4,*)), intent(in) :: b2

        type(base(4,b1%n)) localVal

        localVal = base(4,b1%n)(b1%data, b1%name)

        compareK8K4 = compareK4K4 (localVal, b2)
    end function

    logical function compareK8K8 (c1, c2)
        class(child(8,*)), intent(in) :: c1, c2

        logical(4), external :: precision_r8

        compareK8K8 = precision_r8 (c1%data, c2%data) .and. &
            (c1%name == c2%name) .and. all(c1%rep == c2%rep)
    end function
end module

module m1
use m, only: base, child

    type, extends(child) :: thirdGen (ck)
        integer, kind :: ck = 8

        complex(ck) cx

        contains

        procedure, nopass :: equal8to4 => compareK8K4Forbidden
    end type

    contains

    logical function compareK8K4Forbidden (b1, b2)
        class (base(8,*)), intent(in) :: b1
        class(base(4,*)), intent(in) :: b2

        compareK8K4Forbidden = .false.
    end function
end module

program dtpNopass003
use m1
    real(4), parameter :: pi = 3.1415927
    real(8), parameter :: pi_d = 3.14159265358d0

    type(thirdGen(4,20)) g1(10), g2
    type(thirdGen(8,30)) g3(2,2)

    g1%data = log([(i*pi, i=1,10)])

    g1%name = 'xlftest dtparam'

    g2%data = log (pi*4)
    g2%name = g1(4)%name

    !! test inherited binding equal4to4
    if (.not. g1%equal4to4(g2, g1(4))) error stop 1_4

    if (.not. g2%child%equal4to4(g1(4), g2)) error stop 2_4

    if (g1%equal4to4(g1(2), g2)) error stop 3_4

    g3%data = reshape([log(2*pi), log(3*pi), log(3*pi), log(4*pi)], [2,2])

    g3%name = g1(10)%name

    g3(1,1)%rep = ''
    g3(2,1)%rep = 'A'
    g3(1,2)%rep = 'A'
    g3(2,2)%rep = ''

    !! test inherited binding equal8to8
    if (.not. g1(5)%equal8to8(g3(1,2), g3(2,1))) error stop 4_4

    if (.not. g3(:,2)%equal8to8(g3(1,1), g3(1,1))) error stop 5_4

    if (g3%equal8to8(g3(1,1), g3(2,2))) error stop 6_4

    !! test binding equal8to4
    if (g2%equal8to4(g3(2,2), g1(4))) error stop 7_4

    if (.not. g1(2)%child%equal8to4(g3(2,2), g1(4))) error stop 8_4
end
